module App where

import Database.SQLite.Simple
import Database.Connection

data App = App
  { appDb :: Connection
  }

createApp :: IO App
createApp = do
  conn <- openConnection
  createTables conn
  pure App { appDb = conn }