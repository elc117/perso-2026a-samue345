{-# LANGUAGE OverloadedStrings #-}

module Handler.AppointmentHandlerSpec (spec) where

import Test.Hspec
import Test.Hspec.Wai

import Web.Scotty
import Network.Wai (Application)
import Network.HTTP.Types (methodPost)

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
          "{\"customer\":\"Samuel\",\"machine\":1,\"time\":\"10:00\"}"
          `shouldRespondWith` 201

      it "retorna 409 quando horário já está ocupado" $ do
        request methodPost "/appointments"
          [("Content-Type", "application/json")]
          "{\"customer\":\"Joao\",\"machine\":2,\"time\":\"11:00\"}"
          `shouldRespondWith` 201

        request methodPost "/appointments"
          [("Content-Type", "application/json")]
          "{\"customer\":\"Maria\",\"machine\":2,\"time\":\"11:00\"}"
          `shouldRespondWith` 409