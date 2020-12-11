{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}

module WebNotes.JobSystem 
  ( executeJobs,
    executeInShell,
    JobMonad(..)
  )
where

import WebNotes.Paths
import WebNotes.Template
import WebNotes.ConfigParser
import WebNotes.Utils (explode, doBoth, Extension)

import Prelude hiding (readFile, writeFile)

import Conduit ((.|), runConduit, runResourceT)
import Control.Concurrent hiding (yield)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue
import Control.Exception (bracket)
import Control.Logging (log')
import Control.Monad.RWS.Strict 
import Control.Monad.Reader (ask)
import Control.Monad.State.Strict (get, put, modify)
import Data.Function ((&))
import Data.HashMap.Strict (HashMap, member, (!), empty, insert)
import Data.List (foldl')
import Data.Text (append, pack)
import Data.Text.IO (readFile, writeFile)
import Data.Time (UTCTime(..))
import Fmt (format)
import System.Directory (getAccessTime)
import System.FilePath
import System.Random (randomRs, mkStdGen)
import Turtle (shell, ExitCode(..))
import qualified Data.Conduit.Binary as CB
import qualified Text.Microstache as M
import qualified Turtle as TT


shellExtension = ".sh"
pageExtension = ".html"

type JobState = [Char]

type JobMonad = RWST Config () JobState IO -- It is critical that this state monad is lazy

workFileLength = 8
outputFileLength = 20
charRange = ('a', 'z')

genWorkFile :: Extension -> JobMonad (Item Work)
genWorkFile ext = do
  file <- take workFileLength <$> get
  modify (drop workFileLength)
  return $ toWorkFile $ file <.> ext

genOutputFile :: Extension -> JobMonad (Item Output)
genOutputFile ext = do
  file <- take outputFileLength <$> get
  modify (drop outputFileLength)
  return $ toOutputFile $ file <.> ext


executeInShell command = do
  shellName <- shellName <$> ask
  tempShellFile :: Item Work <- genWorkFile shellExtension
  config <- ask

  let cmd = 
       [shellName, " ", (toPath config tempShellFile)] 
       & map pack 
       & foldl' append " "

  liftIO $ writeFile (toPath config tempShellFile) command
  status <- shell cmd TT.empty
  case status of
    ExitSuccess -> return True
    _           -> return False


data FileDetails = FileDetails 
  { originalFile :: Item Source,
    currentFile :: Item Work
  }

data JobStatus
  = Failed
  | Ongoing (Item Work)
  | Final (Item Output)

executeJob :: Job -> 
              FileDetails ->
              JobMonad JobStatus
executeJob (Convert {..}) (FileDetails {..}) = do

    destFile <- genWorkFile destExt
    config <- ask 

    let sourcePath = toPath config currentFile
    let destPath = toPath config destFile

    log' $ format "Running conversion job for {}: {} -> {}" 
      (toPath config originalFile)
      sourcePath
      destPath

    let command = templateConvert commands $
          ConvertData { sourcePath = sourcePath,
                        destPath = destPath
                      }

    exit <- executeInShell command

    case exit of
      True  -> do
        case final of
          True -> Final <$> copyToOutput destFile
          False -> return $ Ongoing destFile
      False -> return Failed


executeJob (Page {..}) (FileDetails {..}) = do
  config <- ask

  let destFile = toOutputExt originalFile pageExtension 
  let fileName = pack $ getFileName originalFile

  -- For the final file which is converted to
  -- a page, we convert it to an output file
  -- so that is in the repo. 
  
  finalFile <- copyToOutput currentFile

  let origPath = toPath config originalFile
  let sourcePath = toPath config finalFile
  let destPath = toPath config destFile
  
  log' $ format "Running page formation job for {}: {} -> {}" 
    origPath
    sourcePath 
    destPath 
  
  pageContents <- liftIO $ readFile sourcePath
  lastModified <- liftIO $ getAccessTime origPath

  -- NOTE: We need relative paths to pass to pages

  let pageData = templatePage pageTemplate $
        PageData 
          (toRelPath originalFile)
          (toRelPath finalFile)
          pageContents
          fileName
          lastModified
  
  liftIO $ writeFile destPath pageData
  return $ Final destFile


executeJob (BinPage {..}) (FileDetails {..}) = do
  config <- ask

  let destFile = toOutputExt originalFile pageExtension 
  let fileName = pack $ getFileName originalFile

  -- For the final file which is converted to
  -- a page, we convert it to an output file
  -- so that is in the repo. 
  
  finalFile <- copyToOutput currentFile

  let origPath = toPath config originalFile
  let sourcePath = toPath config finalFile
  let destPath = toPath config destFile
  
  log' $ format "Running page formation job for {}: {} -> {}" 
    origPath
    sourcePath 
    destPath 
  
  lastModified <- liftIO $ getAccessTime origPath

  -- NOTE: We need relative paths to pass to pages

  let pageData = templateBinPage pageTemplate $
        BinPageData 
          (toRelPath originalFile)
          (toRelPath finalFile)
          fileName
          lastModified
  
  liftIO $ writeFile destPath pageData
  return $ Final destFile


executeOneJob :: Item Source -> Item Work -> JobMonad JobStatus
executeOneJob originalFile currentFile = do
    config <- ask
    let schm = scheme config
    let (_, _, ext) = explode $ toPath config currentFile
    if not (member ext schm) 
         then return Failed
         else executeJob (schm ! ext) 
                (FileDetails { originalFile = originalFile,
                               currentFile = currentFile
                             })

executeJobs :: Item Source -> JobMonad (Maybe (Item Output))
executeJobs originalFile = do
    put $ randomRs charRange $ mkStdGen (saltFor originalFile) 
    workFile <- copyToWork originalFile
    jobLoop workFile
  where 
    jobLoop currentFile = do
      config <- ask
      state <- executeOneJob originalFile currentFile
      log' $ format "Finished jobs for {}." 
        (toPath config originalFile)
      case state of 
        Failed -> return Nothing
        Final path -> return $ Just path
        Ongoing path -> jobLoop path

copyToWork :: Item Source -> JobMonad (Item Work)
copyToWork originalFile = do
    config <- ask
    destFile <- genWorkFile (sourceExt originalFile)
    let sourcePath = toPath config originalFile
    let destPath = toPath config destFile
    
    liftIO . runResourceT . runConduit $
      (CB.sourceFile sourcePath) .| (CB.sinkFile destPath)

    return destFile

copyToOutput :: Item Work -> JobMonad (Item Output)
copyToOutput workFile = do
    config <- ask
    destFile <- genOutputFile (workExt workFile)
    let sourcePath = toPath config workFile
    let destPath = toPath config destFile
    
    liftIO . runResourceT . runConduit $
      (CB.sourceFile sourcePath) .| (CB.sinkFile destPath)

    return destFile

