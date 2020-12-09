{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}

module Server where

import WebNotes.Template
import WebNotes.JobSystem hiding (workDir, sourceDir)
import WebNotes.ConfigParser as CP
import WebNotes.Notification
import WebNotes.SHA

import Prelude hiding (writeFile)

import Conduit
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
type SHAIndex = HashMap FilePath String
type DestIndex = HashMap FilePath FilePath
type ModIndex = HashMap FilePath UTCTime

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

type MasterMonad = RWST MasterInfo () MasterState IO

-------------------

listFiles :: FilePath -> IO [FilePath]
listFiles path = listDirectory path 
  <&> (map (path </>))
  >>= filterM (\obj -> not <$> (doesDirectoryExist obj))
 

fullScan :: MasterMonad ()
fullScan = do
  log' "Initializing full scan ..."

  source <- sourceDir <$> ask
  files <- liftIO $ listFiles source
  mapM_ fileChanged files


fileChanged :: FilePath -> MasterMonad ()
fileChanged file = do

  log' $ format "Processing {} ..." file

  -- update SHA  

  sha <- liftIO $ computeSHA file
  modify . appShaIndex $ insert file sha

  -- update last modified
  
  lastAccess <- liftIO $ getAccessTime file
  modify . appModIndex $ insert file lastAccess

  log' $ format "SHA: {}, last accessed: {}" sha lastAccess 

  -- execute tasks 
  config <- ask
  destFile <- liftIO $ executeJobs config file

  case destFile of
    Nothing -> do
      warn' $ "No destination file produced!"
      modify . appDestIndex $ delete file
    Just dest -> do
      log' $ format "Destination: {}" destFile
      modify . appDestIndex $ insert file dest



rebuildIndex :: MasterMonad ()
rebuildIndex = do
  log' $ format "Rebuilding index..."

  files <- keys . destIndex <$> get
  modMap <- modIndex <$> get

  let displayPaths = sortOn (modMap !) files
  let indexItems = (flip map) displayPaths $ \path -> 
        IndexItem path (modMap ! path)

  idxTemp <- indexTemplate <$> ask 
  
  let indexFileContents = templateIndex idxTemp $ 
        IndexData { items = indexItems }

  workDir <- workDir <$> ask

  let indexFileDest = workDir </> "index.html"

  liftIO $ writeFile indexFileDest indexFileContents

finalizeCommands :: MasterMonad ()
finalizeCommands = do

  log' "Running final commands ..."

  commands <- finalCommands <$> ask
  workDir <- workDir <$> ask
  shellName <- CP.shellName <$> ask
  exit <- liftIO $ executeInShell commands workDir shellName
  
  when (exit == False) $ 
    error "Finalization commands failed."

--------------------

masterThread :: Channel -> MasterMonad ()
masterThread chan = forever $ do
  msg <- liftIO $ atomically $ readTQueue chan
  liftIO $ print msg

  sourceDir <- sourceDir <$> ask

  log' $ format "Message: {}" (show msg)

  case msg of
    FullScan -> fullScan
    FileChanged file -> fileChanged $ sourceDir </> file

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

  _ <- forkIO $ evalRWST (masterThread chan) config initMasterState 
    >> return ()

  notificationMachineServer port (serverC chan)
