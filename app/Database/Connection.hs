{-# LANGUAGE OverloadedStrings #-}

module Database.Connection where

import Database.SQLite.Simple

openConnection :: IO Connection
openConnection = open "data/laundry.sqlite3"

createTables :: Connection -> IO ()
createTables conn =
  execute_ conn
    "CREATE TABLE IF NOT EXISTS appointments \
    \(id INTEGER PRIMARY KEY AUTOINCREMENT, customer_id INTEGER NOT NULL, machine INTEGER, time TEXT)"
