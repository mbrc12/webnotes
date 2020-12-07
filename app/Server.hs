{-# LANGUAGE OverloadedStrings #-}

module Server where

import WebNotes.Notification
-- import System.Remote.Monitoring

main = do
  -- forkServer "localhost" 8000
  notificationMachineServer 3000 printAndOKServer
