{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}

module Server where

import WebNotes.Paths
import WebNotes.Template
import WebNotes.JobSystem hiding (workDir, sourceDir)
import WebNotes.ConfigParser as CP
import WebNotes.Notification
import WebNotes.SHA

import Prelude hiding (writeFile)

import Conduit hiding (Source)
import Control.Concurrent hiding (yield)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue
import Control.Logging
import Control.Monad (forever, filterM, mapM_, when)
import Control.Monad.RWS.Strict 
import Control.Monad.Reader (ask)
import Control.Monad.State.Strict (get, put, modify)
import Data.List (sortOn)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.HashMap.Strict hiding (map)
import Data.Text.IO (writeFile)
import Data.Text.Lazy (fromStrict)
import qualified Text.Microstache as M
import Data.Time (UTCTime)
import Data.Yaml (decodeFileThrow)
import Options.Applicative hiding (empty)
import System.Directory (listDirectory, doesDirectoryExist, getAccessTime)
import System.FilePath ((</>))
import Fmt (format, formatLn)

-- import System.Remote.Monitoring

--------------------

configRelativePath = "webnotesconf.yaml"

type MasterInfo = Config

type Channel = TQueue Message
type SHAIndex = HashMap (Item Source) String
type DestIndex = HashMap (Item Source) (Item Output)
type ModIndex = HashMap (Item Source) UTCTime

data MasterState = MasterState
  { shaIndex :: SHAIndex,
    destIndex :: DestIndex,
    modIndex :: ModIndex
  }

emptyMasterState = MasterState empty empty empty

appShaIndex f ms
  = ms {shaIndex = f (shaIndex ms)}

appDestIndex f ms
  = ms {destIndex = f (destIndex ms)}

appModIndex f ms
  = ms {modIndex = f (modIndex ms)}

type MasterT = RWST MasterInfo () MasterState
type MasterMonad = MasterT JobMonad

-------------------

listFiles :: MasterMonad [Item Source]
listFiles = do
  sourceDir <- sourceDir <$> ask
  filelist <- liftIO $ listDirectory sourceDir
  valids <- filterM (\obj -> 
      not <$> (liftIO $ 
        doesDirectoryExist (sourceDir </> obj))) $
      filelist
  return $ map toSourceFile valids 

fullScan :: MasterMonad ()
fullScan = do
  log' "Initializing full scan ..."

  source <- sourceDir <$> ask
  files <- listFiles
  mapM_ fileChanged files


fileChanged :: Item Source -> MasterMonad ()
fileChanged item = do

  config <- ask
  let file = toPath config item

  log' $ format "Processing {} ..." file

  -- update SHA  

  sha <- liftIO $ computeSHA file
  modify . appShaIndex $ insert item sha

  -- update last modified
  
  lastAccess <- liftIO $ getAccessTime file
  modify . appModIndex $ insert item lastAccess

  log' $ format "SHA: {}, last accessed: {}" sha lastAccess 

  -- execute tasks 
  config <- ask
  destFile <- lift $ executeJobs item

  case destFile of
    Nothing -> do
      warn' $ "No destination file produced!"
      modify . appDestIndex $ delete item
    Just dest -> do
      log' $ format "Destination: {}" (toPath config dest)
      modify . appDestIndex $ insert item dest



rebuildIndex :: MasterMonad ()
rebuildIndex = do
  log' $ format "Rebuilding index..."

  config <- ask

  destIndex <- destIndex <$> get

  let items = keys destIndex

  modMap <- modIndex <$> get

  let displayItems = sortOn (modMap !) items

  let indexItems = (flip map) displayItems $ \item -> 
        let dest = destIndex ! item
         in IndexItem 
              (toRelPath dest) 
              (getFileName item)
              (modMap ! item)


  idxTemp <- indexTemplate <$> ask 
  
  let indexFileContents = templateIndex idxTemp $ 
        IndexData { items = indexItems }

  workDir <- workDir <$> ask

  let indexFileDest = toPath config (toOutputFile "index.html")

  liftIO $ writeFile indexFileDest indexFileContents

finalizeCommands :: MasterMonad ()
finalizeCommands = do

  log' "Running final commands ..."

  commands <- finalCommands <$> ask
  workDir <- workDir <$> ask
  shellName <- CP.shellName <$> ask
  exit <- lift $ executeInShell commands
  
  when (exit == False) $ 
    error "Finalization commands failed."

--------------------

masterThread :: Channel -> MasterMonad ()
masterThread chan = forever $ do
  msg <- liftIO $ atomically $ readTQueue chan
  -- liftIO $ print msg

  sourceDir <- sourceDir <$> ask

  -- log' $ format "Message: {}" (show msg)

  case msg of
    FullScan -> fullScan
    FileChanged file -> fileChanged file

  rebuildIndex
  finalizeCommands

  log' "Done."


sendMsg :: Channel -> Message -> IO ()
sendMsg chan msg = atomically $ writeTQueue chan msg


serverC :: Channel -> ConduitT Message Response IO ()
serverC chan = awaitForever $ \msg -> do 
  liftIO $ sendMsg chan msg
  yield OK

--------------------

data ServerOptions = ServerOptions 
   { port :: Int 
   }
   deriving Show

fullParser = helper <*> (ServerOptions <$>
  option auto
  (   long "port"
  <>  short 'p'
  <>  help "Port number"
  <>  metavar "PORT"
  ))

executableParser = execParser $ do
  info fullParser $
    (  header "WebNotes - Server"
    )

--------------------

main = withStdoutLogging $ do
  -- Used for ekg diagnostics
  -- forkServer "localhost" 8000
  
  setLogLevel LevelDebug

  (ServerOptions {..}) <- executableParser

  config@(Config {..}) <- decodeFileThrow configRelativePath 

  chan <- newTQueueIO
  sendMsg chan FullScan

  let initMasterState = emptyMasterState

  _ <- forkIO $ 
    evalRWST 
    (evalRWST (masterThread chan) config initMasterState)
    config 
    undefined
    >> return ()

  notificationMachineServer port (serverC chan)
