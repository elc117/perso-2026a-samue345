{-# LANGUAGE OverloadedStrings #-}

module Handler.WaitingQueueHandler where

import Web.Scotty
import Network.HTTP.Types.Status
import Control.Monad.IO.Class

import App
import Models.WaitingQueue
import qualified Service.WaitingQueueService as WaitingQueueService

joinQueueHandler :: App -> ActionM ()
joinQueueHandler app = do
  queue <- jsonData :: ActionM WaitingQueue

  liftIO $
    WaitingQueueService.joinQueue app queue

  status status201

checkDelayHandler :: App -> ActionM ()
checkDelayHandler app = do
  appointmentTime <- queryParam "time"
  currentMinute <- queryParam "current_minute"

  promoted <- liftIO $
    WaitingQueueService.checkDelayAndReleaseQueue
      app
      appointmentTime
      currentMinute

  json promoted