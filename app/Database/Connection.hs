{-# LANGUAGE OverloadedStrings #-}

module Database.Connection where

import Database.SQLite.Simple

openConnection :: IO Connection
openConnection = open "data/laundry.sqlite3"

createTables :: Connection -> IO ()
createTables conn = do
  execute_ conn
    "CREATE TABLE IF NOT EXISTS appointments \
    \(id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \ customer_id INTEGER NOT NULL, \
    \ machine INTEGER, \
    \ scheduled_at TEXT, \
    \ password TEXT NOT NULL, \ 
    \ status TEXT NOT NULL)"

  execute_ conn
    "CREATE TABLE IF NOT EXISTS waiting_queue \
    \(id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \ customer_id INTEGER NOT NULL, \
    \ scheduled_at TEXT NOT NULL, \
    \ created_at TEXT NOT NULL)"