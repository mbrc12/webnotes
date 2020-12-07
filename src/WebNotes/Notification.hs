{-# LANGUAGE OverloadedStrings #-}

module WebNotes.Notification
  ( notificationMachineServer,
    -- notificationMachineClient,
    Message (..),
    Response (..),
    printAndOKServer,
    oneShotClient
  )
where

import Conduit hiding (connect)
import Control.Monad (forever, unless, when)
import Data.Conduit.Network
import Data.Conduit.Text
import Data.Function ((&))
import System.FilePath (FilePath)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as E
import Network.Simple.TCP (Socket, connect, send, recv)

maxLenResponse = 100 :: Int

stopIdentifier = "%%STOP%%"

startIdentifier = "%%START%%" :: T.Text

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
     in sourceC 
     .| decodeUtf8C 
     .| messageConverterC 
     .| serverHandlerC 
     .| responseConverterC 
     .| encodeUtf8C
     .| sinkC 
     &  runConduit
  where
    serverSett = serverSettings port "localhost"

-- getSourceSink :: Int -> (ConduitT () BS.ByteString IO (), ConduitT BS.ByteString Void ())
-- getSourceSink port = 

-- notificationMachineServer' :: Int -> ConduitT Message Response IO () -> IO ()
-- notificationMachineServer' port serverHandlerC = 
--   let (sourceC, sinkC) = getSourceSink port
--    in sourceC .| messageConverterC .| serverHandlerC .| responseConverterC .| sinkC & runConduit 


-- notificationMachineClient :: Int -> ConduitT Response Message IO () -> IO ()
-- notificationMachineClient port clientHandlerC =
--   runTCPClient clientSett $ \appData ->
--     let sourceC = appSource appData
--         sinkC = appSink appData
--      in sourceC
--      .| decodeUtf8C
--      .| responseClientConverterC 
--      .| clientHandlerC 
--      .| messageClientConverterC 
--      .| encodeUtf8C
--      .| sinkC 
--      &  runConduit
--   where
--     clientSett = clientSettings port "localhost"

accumulateTillSubstr :: T.Text -> ConduitT T.Text a IO T.Text
accumulateTillSubstr substr = accumAll ""
  where 
    accumAll acc = do
      msg <- await
      liftIO $ print msg
      case msg of
        Nothing -> return acc
        Just message -> if T.isInfixOf substr message
                          then return (T.append acc message)
                          else accumAll (T.append acc message)

messageConverterC :: ConduitT T.Text Message IO ()
messageConverterC = do
  accum <- accumulateTillSubstr stopIdentifier
  liftIO $ TIO.putStrLn (T.append "got -> " accum)

  if not (T.isInfixOf stopIdentifier accum) -- client stopped
    then return ()
    else if not (T.isInfixOf startIdentifier accum)
          then error $ (T.unpack startIdentifier) ++ " not found before " ++ (T.unpack stopIdentifier)
          else do      
                let (_, rem1) = T.breakOn startIdentifier accum  
                let rem2      = T.drop (T.length startIdentifier) rem1 
                let (msg, _)  = T.breakOn stopIdentifier rem2

                if T.isInfixOf fullScanCommand msg
                   then do
                     yield FullScan
                     messageConverterC
                   else do
                     msg & T.unpack & FileChanged & yield
                     messageConverterC


-- messageClientConverterC :: ConduitT Message T.Text IO ()
-- messageClientConverterC = forever $ do
--   msg <- await
--   case msg of
--     Nothing -> return ()
--     Just msg -> do
--       yield startIdentifier
--       case msg of
--         FileChanged path -> yield $ T.pack path
--         FullScan -> yield fullScanCommand
--       yield stopIdentifier

responseConverterC :: ConduitT Response T.Text IO ()
responseConverterC = awaitForever $ \resp -> resp & show & T.pack & yield

-- responseClientConverterC :: ConduitT T.Text Response IO ()
-- responseClientConverterC = forever $ do
--   resp <- await
--   case resp of
--     Just "OK" -> yield OK
--     _ -> yield Fail

printAndOKServer :: ConduitT Message Response IO ()
printAndOKServer = do
  msg <- await
  liftIO $ print msg
  case msg of
    Nothing -> return ()
    _ -> do 
      yield OK
      printAndOKServer

sendText sock msg = msg & E.encodeUtf8 & send sock
recvText sock len = (fmap E.decodeUtf8) <$> recv sock len 

oneShotClient :: Int -> Message -> IO Response
oneShotClient port msg = 
  connect "localhost" (show port) $ \(sock, _) -> do
    putStrLn "Connected to Server"

    sendText sock startIdentifier
    case msg of
      FullScan -> sendText sock fullScanCommand
      FileChanged path -> sendText sock (T.pack path)
    sendText sock stopIdentifier
    
    resp <- recvText sock maxLenResponse
    case resp of
      Nothing -> return Fail
      Just resp -> if T.isInfixOf "OK" resp
                     then return OK
                     else return Fail
