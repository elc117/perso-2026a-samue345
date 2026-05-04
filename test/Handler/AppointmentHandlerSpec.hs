{-# LANGUAGE OverloadedStrings #-}

module Handler.AppointmentHandlerSpec (spec) where

import Test.Hspec
import Test.Hspec.Wai

import Web.Scotty hiding (request)
import Network.Wai (Application)
import Network.HTTP.Types (methodPost, methodGet)

import Database.SQLite.Simple

import App
import Database.Connection
import qualified Routes.Appointment as AppointmentRoutes

testApplication :: IO Application
testApplication = do
  conn <- open ":memory:"
  createTables conn

  let app = App { appDb = conn }

  scottyApp $
    AppointmentRoutes.routes app

spec :: Spec
spec =
  describe "AppointmentHandler" $ do
    with testApplication $ do
      it "retorna 201 ao criar agendamento" $ do
        request methodPost "/appointments"
          [("Content-Type", "application/json")]
          "{\"customerId\":1,\"machine\":1,\"scheduledAt\":\"2026-05-03 10:00:00\"}"
          `shouldRespondWith` 201

      it "retorna 422 quando scheduledAt está em formato inválido" $ do
        request methodPost "/appointments"
          [("Content-Type", "application/json")]
          "{\"customerId\":1,\"machine\":1,\"scheduledAt\":\"10:00\"}"
          `shouldRespondWith` 422

      it "retorna 409 quando horário já está ocupado" $ do
        request methodPost "/appointments"
          [("Content-Type", "application/json")]
          "{\"customerId\":1,\"machine\":2,\"scheduledAt\":\"2026-05-03 11:00:00\"}"
          `shouldRespondWith` 201

        request methodPost "/appointments"
          [("Content-Type", "application/json")]
          "{\"customerId\":2,\"machine\":2,\"scheduledAt\":\"2026-05-03 11:00:00\"}"
          `shouldRespondWith` 409

      it "admin lista todos os agendamentos" $ do
        request methodPost "/appointments"
          [("Content-Type", "application/json")]
          "{\"customerId\":1,\"machine\":1,\"scheduledAt\":\"2026-05-03 12:00:00\"}"
          `shouldRespondWith` 201

        request methodPost "/appointments"
          [("Content-Type", "application/json")]
          "{\"customerId\":2,\"machine\":2,\"scheduledAt\":\"2026-05-03 13:00:00\"}"
          `shouldRespondWith` 201

        request methodGet "/appointments?role=admin&customer_id=0"
          []
          ""
          `shouldRespondWith` 200

      it "usuário normal lista seus agendamentos" $ do
        request methodPost "/appointments"
          [("Content-Type", "application/json")]
          "{\"customerId\":3,\"machine\":1,\"scheduledAt\":\"2026-05-03 14:00:00\"}"
          `shouldRespondWith` 201

        request methodGet "/appointments?role=customer&customer_id=3"
          []
          ""
          `shouldRespondWith` 200

      it "admin lista com filtro por data" $ do
        request methodPost "/appointments"
          [("Content-Type", "application/json")]
          "{\"customerId\":1,\"machine\":1,\"scheduledAt\":\"2026-05-03 15:00:00\"}"
          `shouldRespondWith` 201

        request methodGet "/appointments?role=admin&date=2026-05-03"
          []
          ""
          `shouldRespondWith` 200

      it "admin lista com filtro por máquina" $ do
        request methodPost "/appointments"
          [("Content-Type", "application/json")]
          "{\"customerId\":1,\"machine\":2,\"scheduledAt\":\"2026-05-03 16:00:00\"}"
          `shouldRespondWith` 201

        request methodGet "/appointments?role=admin&machine=2"
          []
          ""
          `shouldRespondWith` 200

      it "admin lista com filtros por data e máquina" $ do
        request methodPost "/appointments"
          [("Content-Type", "application/json")]
          "{\"customerId\":1,\"machine\":1,\"scheduledAt\":\"2026-05-04 10:00:00\"}"
          `shouldRespondWith` 201

        request methodGet "/appointments?role=admin&date=2026-05-04&machine=1"
          []
          ""
          `shouldRespondWith` 200