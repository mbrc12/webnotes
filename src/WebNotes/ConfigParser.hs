{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module WebNotes.ConfigParser
  (
  )
where

import WebNotes.Utils (Extension, emptyOrUnMaybe)
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
  { shellName :: String,
    workDir :: FilePath,
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

pageActionBuilder :: Extension -> T.Text -> Maybe Action
pageActionBuilder from commands = 
  let templateName = from ++ "-page" & T.pack & PName
      result = MS.compileMustacheText templateName (TL.fromStrict commands)
  in case result of 
       Left _ -> Nothing
       Right template -> Just $ 
         ActionFor from (Page { commands = template })


instance FromJSON Action where
  parseJSON (Object obj) = 
    if member tagIdentifier obj
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


