{-# LANGUAGE OverloadedStrings #-}

module Repositories.WaitingQueueRepository where

import Database.SQLite.Simple
import Models.WaitingQueue

insertQueue :: Connection -> WaitingQueue -> IO ()
insertQueue conn queue =
  execute conn
    "INSERT INTO waiting_queue (customer_id, scheduled_at, created_at) VALUES (?, ?, ?)"
    (customerId queue, scheduledAt queue, createdAt queue)

getQueueByScheduledAt :: Connection -> String -> IO [WaitingQueue]
getQueueByScheduledAt conn scheduledAt =
  query conn
    "SELECT customer_id, scheduled_at, created_at \
    \FROM waiting_queue \
    \WHERE scheduled_at = ? \
    \ORDER BY id ASC"
    (Only scheduledAt)

deleteQueueByScheduledAt :: Connection -> String -> IO ()
deleteQueueByScheduledAt conn scheduledAt =
  execute conn
    "DELETE FROM waiting_queue WHERE scheduled_at = ?"
    (Only scheduledAt)

deleteQueueByCustomerAndScheduledAt :: Connection -> Int -> String -> IO ()
deleteQueueByCustomerAndScheduledAt conn cid scheduledAt =
  execute conn
    "DELETE FROM waiting_queue WHERE customer_id = ? AND scheduled_at = ?"
    (cid, scheduledAt)