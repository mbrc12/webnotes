{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module WebNotes.ConfigParser
  (
    Config(..),
    Action(..),
    Job(..),
    
  )
where

import WebNotes.Utils (Extension, emptyOrUnMaybe)

import Data.Functor ((<&>))
import Data.Function ((&))
import Control.Applicative (empty)
import GHC.Generics
import Text.Microstache as MS
import Data.Yaml
import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Text.Microstache as M
import Data.HashMap.Strict (HashMap, member, (!), insert)
import qualified Data.HashMap.Strict as M

data Action = ActionFor Extension Job
  deriving Show

type JobScheme = HashMap Extension Job


data Job 
  = Convert 
    { destExt :: Extension,
      commands  :: M.Template
    }
  | Page 
    {
      pageTemplate :: M.Template
    }  
    deriving Show

data Config = Config
  { shellName :: String,
    sourceDir :: FilePath,
    workDir :: FilePath,
    outputDir :: FilePath,
    indexTemplate :: M.Template,
    finalCommands :: T.Text,
    scheme :: JobScheme
  } deriving (Show, Generic)


formulateJobScheme :: [Action] -> JobScheme
formulateJobScheme =
  foldl' 
    (\map (ActionFor ext job) ->
      insert ext job map) 
    M.empty


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
         ActionFor from (Page { pageTemplate = template })


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
                <*> obj .: "template"
                & emptyOrUnMaybe
        else empty

  parseJSON invalid = empty

instance FromJSON Config where
  parseJSON (Object obj) = Config
    <$> obj .: "shell_name"
    <*> obj .: "source_dir"
    <*> obj .: "work_dir"
    <*> obj .: "output_dir"
    <*> (   obj .: "index_template"
        <&> indexTemplateCompiler)
    <*> obj .: "final_commands"
    <*> (   obj .: "conversions" 
        <&> formulateJobScheme)

  parseJSON invalid = empty

indexTemplateCompiler :: T.Text -> M.Template
indexTemplateCompiler indexText = 
  let result = M.compileMustacheText "index-template" $ TL.fromStrict indexText
   in case result of 
        Left _ -> error "Couldn't compile index template"
        Right template -> template

