{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}

module Server where

import WebNotes.Template
import WebNotes.JobSystem hiding (workDir)
import WebNotes.ConfigParser as CP
import WebNotes.Notification
import WebNotes.SHA

import Prelude hiding (writeFile)

import Conduit
import Control.Concurrent hiding (yield)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue
import Control.Monad (forever, filterM, mapM_, when)
import Control.Monad.RWS.Strict 
import Control.Monad.Reader (ask)
import Control.Monad.State.Strict (get, put, modify)
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

-- import System.Remote.Monitoring

--------------------

configRelativePath = "webnotesconf.yaml"

data MasterInfo = MasterInfo 
  { config :: ConfigFile,
    jobScheme :: JobScheme,
    indexTemplate :: M.Template
  }

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
  source <- sourceDir . config <$> ask
  files <- liftIO $ listFiles source
  mapM_ fileChanged files


fileChanged :: FilePath -> MasterMonad ()
fileChanged file = do
 
  -- update SHA  

  sha <- liftIO $ computeSHA file
  modify . appShaIndex $ insert file sha

  -- update last modified
  
  lastAccess <- liftIO $ getAccessTime file
  modify . appModIndex $ insert file lastAccess

  -- execute tasks 
  scheme <- jobScheme <$> ask
  destFile <- liftIO $ executeJobs scheme file

  case destFile of
    Nothing -> modify . appDestIndex $ delete file
    Just dest -> modify . appDestIndex $ insert file dest


sortByValuesInSecond :: (Ord b) => [a] -> HashMap a b -> [a]
sortByValuesInSecond = undefined

rebuildIndex :: MasterMonad ()
rebuildIndex = do
  files <- keys . destIndex <$> get
  modMap <- modIndex <$> get

  let displayPaths = sortByValuesInSecond files modMap
  let indexItems = (flip map) displayPaths $ \path -> 
        IndexItem path (modMap ! path)

  idxTemp <- indexTemplate <$> ask 
  
  let indexFileContents = templateIndex idxTemp $ 
        IndexData { items = indexItems }

  workDir <- workDir . config <$> ask

  let indexFileDest = workDir </> "index.html"

  liftIO $ writeFile indexFileDest indexFileContents

finalizeCommands :: MasterMonad ()
finalizeCommands = do
  commands <- finalCommands . config <$> ask
  workDir <- workDir . config <$> ask
  shellName <- CP.shellName . config <$> ask
  exit <- liftIO $ executeInShell commands workDir shellName
  
  when (exit == False) $ 
    error "Finalization commands failed."

--------------------

masterThread :: Channel -> MasterMonad ()
masterThread chan = forever $ do
  msg <- liftIO $ atomically $ readTQueue chan
  liftIO $ print msg

  sourceDir <- sourceDir . config <$> ask

  case msg of
    FullScan -> fullScan
    FileChanged file -> fileChanged $ sourceDir </> file

  rebuildIndex
  finalizeCommands


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

main = do
  -- Used for ekg diagnostics
  -- forkServer "localhost" 8000
  
  (ServerOptions {..}) <- executableParser

  configFile@(ConfigFile {..}) <- decodeFileThrow configRelativePath 

  chan <- newTQueueIO
  sendMsg chan FullScan

  let initMasterState = emptyMasterState
  let scheme = formulateJobScheme workDir shellName conversions

  let masterInfo = MasterInfo configFile scheme $ 
        either 
          (error "Couldn't compile index template")
          id $ 
            M.compileMustacheText "index-template" $ 
              fromStrict indexTemplateRaw

  _ <- forkIO $ evalRWST (masterThread chan) masterInfo initMasterState 
    >> return ()

  notificationMachineServer port (serverC chan)
