{-# LANGUAGE OverloadedStrings #-}

module WebNotes.Notification
  ( notificationMachineServer,
    notificationMachineClient,
    Message (..),
    Response (..),
    printAndOKServer,
  )
where

import Conduit
import Control.Monad (forever)
import Data.Conduit.Network
import Data.Function ((&))
import System.FilePath (FilePath)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

stopIdentifier = "%%STOP%%"

startIdentifier = "%%START%%"

fullScanCommand = "%%FULLSCAN%%"

data Message
  = FileChanged FilePath
  | FullScan
  deriving (Show)

data Response
  = OK
  | Fail
  deriving (Show)

notificationMachineServer :: Int -> ConduitT Message Response IO () -> IO ()
notificationMachineServer port serverHandlerC =
  runTCPServer serverSett $ \appData ->
    let sourceC = appSource appData
        sinkC = appSink appData
     in sourceC .| messageConverterC .| serverHandlerC .| responseConverterC .| sinkC & runConduit
  where
    serverSett = serverSettings port "*"

notificationMachineClient :: Int -> ConduitT Response Message IO () -> IO ()
notificationMachineClient port clientHandlerC =
  runTCPClient clientSett $ \appData ->
    let sourceC = appSource appData
        sinkC = appSink appData
     in sourceC .| responseClientConverterC .| clientHandlerC .| messageClientConverterC .| sinkC & runConduit
  where
    clientSett = clientSettings port "*"

accumulateTillSubstr :: T.Text -> ConduitT BS.ByteString a IO T.Text
accumulateTillSubstr substr = accumAll ""
  where 
    accumAll acc = do
    start <- (fmap E.decodeUtf8) <$> await
    case start of
      Nothing -> return acc
      Just message -> if T.isInfixOf substr message
                        then return acc
                        else accumAll (T.append acc message)

messageConverterC :: ConduitT BS.ByteString Message IO ()
messageConverterC = do
  _ <- accumulateTillSubstr startIdentifier
  message <- accumulateTillSubstr stopIdentifier
  if T.isInfixOf fullScanCommand message
    then yield FullScan
    else do
      message & T.unpack & FileChanged & yield
      messageConverterC

yieldText = yield . E.encodeUtf8

messageClientConverterC :: ConduitT Message BS.ByteString IO ()
messageClientConverterC = forever $ do
  msg <- await
  case msg of
    Nothing -> return ()
    Just msg -> do
      yieldText startIdentifier
      case msg of
        FileChanged path -> yieldText $ T.pack path
        FullScan -> yieldText fullScanCommand
      yieldText stopIdentifier

responseConverterC :: ConduitT Response BS.ByteString IO ()
responseConverterC = forever $ do
  resp <- await
  case resp of
    Nothing -> return ()
    _ -> resp & show & T.pack & E.encodeUtf8 & yield

responseClientConverterC :: ConduitT BS.ByteString Response IO ()
responseClientConverterC = forever $ do
  resp <- (fmap E.decodeUtf8) <$> await
  case resp of
    Just "OK" -> yield OK
    _ -> yield Fail

printAndOKServer :: ConduitT Message Response IO ()
printAndOKServer = do
  msg <- await
  liftIO $ print msg
  case msg of
    Nothing -> yield OK
    _ -> printAndOKServer
