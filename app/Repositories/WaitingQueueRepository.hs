{-# LANGUAGE OverloadedStrings #-}

module Repositories.WaitingQueueRepository where

import Database.SQLite.Simple
import Models.WaitingQueue

insertQueue :: Connection -> WaitingQueue -> IO ()
insertQueue conn queue =
  execute conn
    "INSERT INTO waiting_queue (customer_id, machine, time, created_at) VALUES (?, ?, ?, ?)"
    (customerId queue, machine queue, time queue, createdAt queue)

getQueueByTime :: Connection -> String -> IO [WaitingQueue]
getQueueByTime conn appointmentTime =
  query conn
    "SELECT customer_id, machine, time, created_at \
    \FROM waiting_queue \
    \WHERE time = ? \
    \ORDER BY id ASC"
    (Only appointmentTime)

deleteQueueByTime :: Connection -> String -> IO ()
deleteQueueByTime conn appointmentTime =
  execute conn
    "DELETE FROM waiting_queue WHERE time = ?"
    (Only appointmentTime)

deleteQueueByCustomerAndTime :: Connection -> Int -> String -> IO ()
deleteQueueByCustomerAndTime conn cid appointmentTime =
  execute conn
    "DELETE FROM waiting_queue WHERE customer_id = ? AND time = ?"
    (cid, appointmentTime)