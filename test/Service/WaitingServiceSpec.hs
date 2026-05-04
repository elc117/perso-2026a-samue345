{-# LANGUAGE OverloadedStrings #-}

module Service.WaitingQueueServiceSpec (spec) where

import Test.Hspec
import Database.SQLite.Simple
import Data.Time

import App
import Database.Connection
import DTO.CreateAppointmentRequest
import Models.WaitingQueue

import qualified Repositories.AppointmentRepository as AppointmentRepository
import qualified Repositories.WaitingQueueRepository as WaitingQueueRepository
import qualified Service.WaitingQueueService as WaitingQueueService

scheduledAt :: String
scheduledAt = "2026-05-03 10:00:00"

testNow :: ZonedTime
testNow =
  ZonedTime
    (LocalTime (fromGregorian 2026 5 3) (TimeOfDay 10 8 0))
    (hoursToTimeZone (-3))

spec :: Spec
spec = do
  describe "WaitingQueueService checkDelayAndReleaseQueueAt" $ do
    it "substitui cliente atrasado pelo primeiro da fila após tolerância" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      AppointmentRepository.insertAppointment conn
        CreateAppointmentRequest
          { customerId = 1
          , machine = 1
          , scheduledAt = LocalTime (fromGregorian 2026 5 3) (TimeOfDay 10 0 0)
          }
        "1234"

      WaitingQueueRepository.insertQueue conn
        WaitingQueue
          { customerId = 2
          , scheduledAt = scheduledAt
          , createdAt = "2026-05-03 09:50:00"
          }

      WaitingQueueService.checkDelayAndReleaseQueueAt app testNow

      Just appointment <- AppointmentRepository.findAppointmentById conn 1

      customerId appointment `shouldBe` 2

      queue <- WaitingQueueRepository.getQueueByScheduledAt conn scheduledAt

      queue `shouldBe` []

    it "não substitui antes da tolerância de atraso" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      AppointmentRepository.insertAppointment conn
        CreateAppointmentRequest
          { customerId = 1
          , machine = 1
          , scheduledAt = LocalTime (fromGregorian 2026 5 3) (TimeOfDay 10 0 0)
          }
        "1234"

      WaitingQueueRepository.insertQueue conn
        WaitingQueue
          { customerId = 2
          , scheduledAt = scheduledAt
          , createdAt = "2026-05-03 09:50:00"
          }

      let beforeTolerance =
            ZonedTime
              (LocalTime (fromGregorian 2026 5 3) (TimeOfDay 10 7 0))
              (hoursToTimeZone (-3))

      WaitingQueueService.checkDelayAndReleaseQueueAt app beforeTolerance

      Just appointment <- AppointmentRepository.findAppointmentById conn 1

      customerId appointment `shouldBe` 1

    it "usa a fila em ordem de chegada" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      AppointmentRepository.insertAppointment conn
        CreateAppointmentRequest
          { customerId = 1
          , machine = 1
          , scheduledAt = LocalTime (fromGregorian 2026 5 3) (TimeOfDay 10 0 0)
          }
        "1234"

      WaitingQueueRepository.insertQueue conn
        WaitingQueue
          { customerId = 2
          , scheduledAt = scheduledAt
          , createdAt = "2026-05-03 09:50:00"
          }

      WaitingQueueRepository.insertQueue conn
        WaitingQueue
          { customerId = 3
          , scheduledAt = scheduledAt
          , createdAt = "2026-05-03 09:55:00"
          }

      WaitingQueueService.checkDelayAndReleaseQueueAt app testNow

      Just appointment <- AppointmentRepository.findAppointmentById conn 1

      customerId appointment `shouldBe` 2
    it "não substitui agendamento de mesmo horário em outro dia" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      AppointmentRepository.insertAppointment conn
        CreateAppointmentRequest
          { customerId = 1
          , machine = 1
          , scheduledAt = LocalTime (fromGregorian 2026 5 3) (TimeOfDay 10 0 0)
          }
        "1234"

      AppointmentRepository.insertAppointment conn
        CreateAppointmentRequest
          { customerId = 9
          , machine = 1
          , scheduledAt = LocalTime (fromGregorian 2026 5 4) (TimeOfDay 10 0 0)
          }
        "1234"

      WaitingQueueRepository.insertQueue conn
        WaitingQueue
          { customerId = 2
          , scheduledAt = "2026-05-03 10:00:00"
          , createdAt = "2026-05-03 09:50:00"
          }

      WaitingQueueService.checkDelayAndReleaseQueueAt app testNow

      Just todayAppointment <- AppointmentRepository.findAppointmentById conn 1
      Just tomorrowAppointment <- AppointmentRepository.findAppointmentById conn 2

      customerId todayAppointment `shouldBe` 2
      customerId tomorrowAppointment `shouldBe` 9
    it "não substitui agendamento de outro horário no mesmo dia" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      AppointmentRepository.insertAppointment conn
        CreateAppointmentRequest
          { customerId = 1
          , machine = 1
          , scheduledAt = LocalTime (fromGregorian 2026 5 3) (TimeOfDay 10 0 0)
          }
        "1234"

      AppointmentRepository.insertAppointment conn
        CreateAppointmentRequest
          { customerId = 9
          , machine = 1
          , scheduledAt = LocalTime (fromGregorian 2026 5 3) (TimeOfDay 11 0 0)
          }
        "1234"

      WaitingQueueRepository.insertQueue conn
        WaitingQueue
          { customerId = 2
          , scheduledAt = "2026-05-03 10:00:00"
          , createdAt = "2026-05-03 09:50:00"
          }

      WaitingQueueService.checkDelayAndReleaseQueueAt app testNow

      Just tenAppointment <- AppointmentRepository.findAppointmentById conn 1
      Just elevenAppointment <- AppointmentRepository.findAppointmentById conn 2

      customerId tenAppointment `shouldBe` 2
      customerId elevenAppointment `shouldBe` 9
      