{-# LANGUAGE OverloadedStrings #-}

module Handler.AppointmentHandlerSpec (spec) where

import Test.Hspec
import Test.Hspec.Wai

import Web.Scotty
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
          "{\"customerId\":1,\"machine\":1,\"time\":\"10:00\"}"
          `shouldRespondWith` 201

      it "retorna 409 quando horário já está ocupado" $ do
        request methodPost "/appointments"
          [("Content-Type", "application/json")]
          "{\"customerId\":1,\"machine\":2,\"time\":\"11:00\"}"
          `shouldRespondWith` 201

        request methodPost "/appointments"
          [("Content-Type", "application/json")]
          "{\"customerId\":2,\"machine\":2,\"time\":\"11:00\"}"
          `shouldRespondWith` 409

      it "admin lista todos os agendamentos" $ do
        request methodPost "/appointments"
          [("Content-Type", "application/json")]
          "{\"customerId\":1,\"machine\":1,\"time\":\"12:00\"}"
          `shouldRespondWith` 201

        request methodPost "/appointments"
          [("Content-Type", "application/json")]
          "{\"customerId\":2,\"machine\":2,\"time\":\"13:00\"}"
          `shouldRespondWith` 201

        request methodGet "/appointments?role=admin&customer_id=0"
          []
          ""
          `shouldRespondWith` 200

      it "usuário normal lista seus agendamentos" $ do
        request methodPost "/appointments"
          [("Content-Type", "application/json")]
          "{\"customerId\":3,\"machine\":1,\"time\":\"14:00\"}"
          `shouldRespondWith` 201

        request methodGet "/appointments?customer_id=3"
          []
          ""
          `shouldRespondWith` 200