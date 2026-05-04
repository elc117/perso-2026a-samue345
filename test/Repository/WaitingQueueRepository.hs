{-# LANGUAGE OverloadedStrings #-}

module Repositories.WaitingQueueRepositorySpec (spec) where

import Test.Hspec
import Database.SQLite.Simple

import Database.Connection
import Models.WaitingQueue

import qualified Repositories.WaitingQueueRepository as WaitingQueueRepository

queueItem :: Int -> String -> String -> WaitingQueue
queueItem cid scheduled created =
  WaitingQueue
    { customerId = cid
    , scheduledAt = scheduled
    , createdAt = created
    }

spec :: Spec
spec = do
  describe "WaitingQueueRepository" $ do
    it "insere e busca fila por scheduled_at" $ do
      conn <- open ":memory:"
      createTables conn

      WaitingQueueRepository.insertQueue conn $
        queueItem 1 "2026-05-03 10:00:00" "2026-05-03 09:50:00"

      result <- WaitingQueueRepository.getQueueByScheduledAt conn "2026-05-03 10:00:00"

      result `shouldBe`
        [queueItem 1 "2026-05-03 10:00:00" "2026-05-03 09:50:00"]

    it "retorna fila em ordem de inserção" $ do
      conn <- open ":memory:"
      createTables conn

      WaitingQueueRepository.insertQueue conn $
        queueItem 1 "2026-05-03 10:00:00" "2026-05-03 09:55:00"

      WaitingQueueRepository.insertQueue conn $
        queueItem 2 "2026-05-03 10:00:00" "2026-05-03 09:50:00"

      result <- WaitingQueueRepository.getQueueByScheduledAt conn "2026-05-03 10:00:00"

      map customerId result `shouldBe` [1, 2]

    it "não mistura mesmo horário em dias diferentes" $ do
      conn <- open ":memory:"
      createTables conn

      WaitingQueueRepository.insertQueue conn $
        queueItem 1 "2026-05-03 10:00:00" "2026-05-03 09:50:00"

      WaitingQueueRepository.insertQueue conn $
        queueItem 2 "2026-05-04 10:00:00" "2026-05-04 09:50:00"

      result <- WaitingQueueRepository.getQueueByScheduledAt conn "2026-05-03 10:00:00"

      map customerId result `shouldBe` [1]

    it "não mistura horários diferentes no mesmo dia" $ do
      conn <- open ":memory:"
      createTables conn

      WaitingQueueRepository.insertQueue conn $
        queueItem 1 "2026-05-03 10:00:00" "2026-05-03 09:50:00"

      WaitingQueueRepository.insertQueue conn $
        queueItem 2 "2026-05-03 11:00:00" "2026-05-03 10:50:00"

      result <- WaitingQueueRepository.getQueueByScheduledAt conn "2026-05-03 10:00:00"

      map customerId result `shouldBe` [1]

    it "deleta toda a fila de um scheduled_at" $ do
      conn <- open ":memory:"
      createTables conn

      WaitingQueueRepository.insertQueue conn $
        queueItem 1 "2026-05-03 10:00:00" "2026-05-03 09:50:00"

      WaitingQueueRepository.insertQueue conn $
        queueItem 2 "2026-05-03 10:00:00" "2026-05-03 09:55:00"

      WaitingQueueRepository.deleteQueueByScheduledAt conn "2026-05-03 10:00:00"

      result <- WaitingQueueRepository.getQueueByScheduledAt conn "2026-05-03 10:00:00"

      result `shouldBe` []

    it "deleta apenas cliente específico de um scheduled_at" $ do
      conn <- open ":memory:"
      createTables conn

      WaitingQueueRepository.insertQueue conn $
        queueItem 1 "2026-05-03 10:00:00" "2026-05-03 09:50:00"

      WaitingQueueRepository.insertQueue conn $
        queueItem 2 "2026-05-03 10:00:00" "2026-05-03 09:55:00"

      WaitingQueueRepository.deleteQueueByCustomerAndScheduledAt conn 1 "2026-05-03 10:00:00"

      result <- WaitingQueueRepository.getQueueByScheduledAt conn "2026-05-03 10:00:00"

      map customerId result `shouldBe` [2]
      