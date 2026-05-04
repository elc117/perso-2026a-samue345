{-# LANGUAGE OverloadedStrings #-}

module Routes.WaitingQueue where

import Web.Scotty

import App
import Handler.WaitingQueueHandler

routes :: App -> ScottyM ()
routes app = do
  post "/queue" $
    joinQueueHandler app

  post "/queue/check-delay" $
    checkDelayHandler app
    