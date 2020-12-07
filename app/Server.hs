{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Server where

import WebNotes.JobSystem
import WebNotes.ConfigParser
import WebNotes.Notification

import Data.Function ((&))
import Conduit
import Control.Monad (forever)
import Control.Concurrent hiding (yield)
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM (atomically)
import Control.Monad.RWS.Strict 
import Data.HashMap.Strict
import Data.Yaml (decodeFileThrow)

-- import System.Remote.Monitoring

configRelativePath = "webnotesconf.yaml"

type Channel = TQueue Message
type SHAIndex = HashMap FilePath String

masterThread :: Channel -> RWST ConfigFile () SHAIndex IO ()
masterThread queue = forever $ do
  msg <- liftIO $ atomically $ readTQueue queue
  liftIO $ print msg





serverC :: Channel -> ConduitT Message Response IO ()
serverC queue = awaitForever $ \msg -> do 
  liftIO $ atomically $ writeTQueue queue msg
  yield OK

main = do
  -- Used for ekg diagnostics
  -- forkServer "localhost" 8000
  
  let shaIndex = empty
  configFile :: ConfigFile <- decodeFileThrow configRelativePath 

  queue <- newTQueueIO
  _ <- forkIO $ evalRWST (masterThread queue) configFile shaIndex >> return ()

  notificationMachineServer 3000 (serverC queue)
