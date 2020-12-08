{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module WebNotes.JobSystem 
  ( Action (..),
    Job(..),
    JobScheme (..),
    formulateJobScheme,
    executeJobs,
    executeInShell
  )
where

import WebNotes.Template
import WebNotes.Utils (explode, doBoth, Extension)

import Prelude hiding (readFile, writeFile)

import Data.Function ((&))
import Control.Exception (bracket)
import System.FilePath
import System.Directory (getAccessTime)
import Data.Time (UTCTime(..))
import Data.HashMap.Strict (HashMap, member, (!), empty, insert)
import Data.List (foldl')
import Data.Text (append, pack)
import Data.Text.IO (readFile, writeFile)
import Turtle (shell, ExitCode(..))
import qualified Text.Microstache as M
import qualified Turtle as TT


tempShellFile = ".tempWebNotes"
pageExtension = "html"

data Action = ActionFor Extension Job
  deriving Show

data JobScheme 
  = JobScheme 
    { scheme :: HashMap Extension Job,
      shellName :: String,
      workDir :: FilePath
    } 
    deriving Show

data Job 
  = Convert 
    { destExt :: Extension,
      commands  :: M.Template
    }
  | Page 
    {
      commands :: M.Template
    }  
    deriving Show

formulateJobScheme :: FilePath -> String -> [Action] -> JobScheme
formulateJobScheme workDir shellName actions = 
  JobScheme 
    { workDir = workDir,
      shellName = shellName,
      scheme  = mapOfJobScheme actions 
    }


mapOfJobScheme :: [Action] -> HashMap Extension Job
mapOfJobScheme =
  foldl' 
    (\map (ActionFor ext job) ->
      insert ext job map) 
    empty



executeInShell command workDir shellName = 
  let tempPath = workDir </> tempShellFile
      cmd      = [shellName, " ", tempPath] & map pack & foldl' append " "
   in do
     bracket 
      (writeFile tempPath command) 
      (const $ return False) $  
        const $ do
          status <- shell cmd TT.empty
          case status of
            ExitSuccess -> return True
            _           -> return False


data FileDetails = FileDetails 
  { originalFile :: FilePath,
    currentFile :: FilePath,
    fileName :: String
  }

data JobState 
  = Failed
  | Ongoing FilePath
  | Final FilePath

executeJob :: Job -> 
              FileDetails ->
              String ->
              String ->
              IO JobState
executeJob (Convert {..}) (FileDetails {..}) workDir shellName = 
  let destPath = workDir </> fileName </> destExt
      command = templateConvert commands $
        ConvertData { sourcePath = currentFile,
                      destPath = destPath 
                    }
  in do
    exit <- executeInShell command workDir shellName
    return $ case exit of
               True  -> Ongoing destPath
               False -> Failed

executeJob (Page {..}) (FileDetails {..}) workDir shellName = 
  let destPath = workDir </> fileName </> pageExtension
  in do
    bracket 
      (doBoth 
        (readFile currentFile)
        (getAccessTime originalFile))
      (const $ return Failed) $ \(pageContents, lastModified) -> do
        let pageContents = templatePage commands $
              PageData { originalPath = originalFile,
                         finalPath = currentFile,
                         pageContents = pageContents,
                         lastModified = lastModified
                       }
        -- exit <- executeInShell command workDir shellName
        writeFile destPath pageContents
        return $ Final destPath
        -- return $ case exit of 
        --            True ->  Final destPath
        --            False -> Failed


executeOneJob :: JobScheme -> FilePath -> FilePath -> IO JobState
executeOneJob (JobScheme {..}) originalFile currentFile = 
  let (dir, fileName, ext) = explode currentFile
  in  if not (member ext scheme) 
         then return Failed
         else executeJob (scheme ! ext) 
                (FileDetails { originalFile = originalFile,
                               currentFile = currentFile,
                               fileName = fileName
                             })
                workDir
                shellName 
                 
executeJobs :: JobScheme -> FilePath -> IO (Maybe FilePath)
executeJobs scheme originalFile = jobLoop originalFile
  where 
    jobLoop currentFile = do
      state <- executeOneJob scheme originalFile currentFile
      case state of 
        Failed -> return Nothing
        Final path -> return $ Just path
        Ongoing path -> jobLoop path
