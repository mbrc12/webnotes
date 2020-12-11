{-# LANGUAGE RecordWildCards #-}

module Client where

import WebNotes.Notification

import Options.Applicative
import Conduit

data ClientOptions = ClientOptions
  { port :: Int,
    message :: Message'
  } 
  deriving Show

fileChangedParser :: Parser Message'
fileChangedParser = FileChanged' <$> strOption
  (   long "file"
  <>  short 'f'
  <>  help "Inform the server that this file has (probably) changed"
  <>  completer (bashCompleter "file")
  <>  metavar "FILENAME"
  )

fullScanParser :: Parser Message'
fullScanParser = flag' FullScan'
  (   long "fullscan"
  <>  short 's'
  <>  help "Do a full scan of the source directory for changed files"
  )

messageParser = fileChangedParser <|> fullScanParser

clientParser = ClientOptions <$> 
  option auto
  (   long "port"
  <>  short 'p'
  <>  help "Port number"
  <>  metavar "PORT"
  )
  <*> 
  messageParser  

fullParser = helper <*> clientParser

executableParser = execParser $ do
  info fullParser $
    (  header "WebNotes - Client"
    )

-- oneShot :: Message -> ConduitT Response Message IO ()
-- oneShot msg = do
--   yield msg
--   resp <- await
--   liftIO $ putStrLn $ show resp

main = do
  (ClientOptions {..}) <- executableParser
  resp <- oneShotClient port message 
  print resp
