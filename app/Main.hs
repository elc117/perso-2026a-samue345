module Main where

import Web.Scotty
import App
import qualified Web as AppWeb

main :: IO ()
main = do
  app <- createApp
  scotty 3000 (AppWeb.routes app)
  