{-# LANGUAGE OverloadedStrings, DeriveGeneric, ApplicativeDo #-}

module WebNotes.ConfigParser
  (
  )
where

import WebNotes.Utils (Extension)
import WebNotes.JobSystem

import Data.Function ((&))
import Control.Applicative (empty)
import GHC.Generics
import Text.Microstache as MS
import Data.Yaml
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.HashMap.Strict (HashMap, member, (!))

data ConfigFile = ConfigFile 
  { shellName :: T.Text,
    workDir :: T.Text,
    indexTemplate :: T.Text,
    pageTemplate :: T.Text,
    finalCommands :: T.Text,
    conversions :: [Action]
  } deriving (Show, Generic)

tagIdentifier = "type"

convertActionBuilder :: Extension -> Extension -> T.Text -> Maybe Action
convertActionBuilder from to commands = 
  let templateName = from ++ "-convert" & T.pack & PName
      result = MS.compileMustacheText templateName (TL.fromStrict commands)
  in case result of 
       Left _ -> Nothing
       Right template -> Just $ 
         ActionFor from (Convert { destExt = to, commands = template })

pageActionBuilder :: Extension -> Extension -> T.Text -> Maybe Action
pageActionBuilder from to commands = 
  let templateName = from ++ "-page" & T.pack & PName
      result = MS.compileMustacheText templateName (TL.fromStrict commands)
  in case result of 
       Left _ -> Nothing
       Right template -> Just $ 
         ActionFor from (Page { commands = template })


emptyOrUnMaybe :: (Applicative m) => m (Either a b) -> m b
emptyOrUnMaybe x = do
  z <- x
  case z of 
    Left _ -> empty
    Right y -> return y

instance FromJSON Action where
  parseJSON (Object obj) = 
    if member obj tagIdentifier
       then case obj ! tagIdentifier of
              String "convert" -> convertActionBuilder
                <$> obj .: "from"
                <*> obj .: "to"
                <*> obj .: "commands"
                & emptyOrUnMaybe
              String "page" -> pageActionBuilder
                <$> obj .: "from"
                <*> obj .: "commands"
                & emptyOrUnMaybe
        else empty

  parseJSON invalid = empty

instance FromJSON ConfigFile where
  parseJSON (Object obj) = ConfigFile 
    <$> obj .: "shell_name"
    <*> obj .: "work_dir"
    <*> obj .: "index_template"
    <*> obj .: "page_template"
    <*> obj .: "final_commands"
    <*> obj .: "conversions"

  parseJSON invalid = empty

