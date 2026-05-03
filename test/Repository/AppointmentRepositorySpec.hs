{-# LANGUAGE OverloadedStrings #-}

module Service.AppointmentServiceSpec (spec) where

import Test.Hspec
import Database.SQLite.Simple
import Data.Time

import App
import Database.Connection
import Models.Appointment
import DTO.CreateAppointmentRequest

import qualified Repositories.AppointmentRepository as AppointmentRepository
import qualified Service.AppointmentService as AppointmentService

scheduled :: Int -> LocalTime
scheduled hour =
  LocalTime (fromGregorian 2026 5 3) (TimeOfDay hour 0 0)

spec :: Spec
spec = do
  describe "AppointmentService createAppointment" $ do
    it "cria um agendamento quando horário está livre" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      let request = CreateAppointmentRequest
            { customerId = 1
            , machine = 1
            , scheduledAt = scheduled 10
            }

      result <- AppointmentService.createAppointment app request

      result `shouldSatisfy` either (const False) (const True)

    it "bloqueia agendamento duplicado na mesma máquina e horário" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      let request = CreateAppointmentRequest
            { customerId = 1
            , machine = 1
            , scheduledAt = scheduled 10
            }

      _ <- AppointmentService.createAppointment app request
      result <- AppointmentService.createAppointment app request

      result `shouldBe` Left "Essa máquina já está ocupada nesse horário."