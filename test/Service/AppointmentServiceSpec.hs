{-# LANGUAGE OverloadedStrings #-}

module Service.AppointmentServiceSpec (spec) where

import Test.Hspec
import Database.SQLite.Simple
import Data.Time

import App
import Database.Connection

import qualified Models.Appointment as Appointment
import qualified DTO.CreateAppointmentRequest as Req
import qualified DTO.AppointmentQuery as Query

import qualified Repositories.AppointmentRepository as AppointmentRepository
import qualified Service.AppointmentService as AppointmentService

scheduled :: Int -> Int -> LocalTime
scheduled day hour =
  LocalTime (fromGregorian 2026 5 day) (TimeOfDay hour 0 0)

request :: Int -> Int -> Int -> Int -> Req.CreateAppointmentRequest
request cid mach day hour =
  Req.CreateAppointmentRequest
    { Req.customerId = cid
    , Req.machine = mach
    , Req.scheduledAt = scheduled day hour
    }

spec :: Spec
spec = do
  describe "AppointmentService createAppointment" $ do
    it "cria um agendamento quando horário está livre" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }
      let req = request 1 1 3 10

      result <- AppointmentService.createAppointment app req

      result `shouldSatisfy` either (const False) (const True)

    it "bloqueia agendamento duplicado na mesma máquina e horário" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }
      let req = request 1 1 3 10

      _ <- AppointmentService.createAppointment app req
      result <- AppointmentService.createAppointment app req

      result `shouldBe` Left "Essa máquina já está ocupada nesse horário."

  describe "AppointmentService listAppointments" $ do
    it "admin lista todos os agendamentos" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      AppointmentRepository.insertAppointment conn (request 1 1 3 10) "1234"
      AppointmentRepository.insertAppointment conn (request 2 2 3 11) "1234"

      let filters =
            Query.AppointmentQuery
              { Query.isAdmin = True
              , Query.customerId = Nothing
              , Query.date = Nothing
              , Query.machine = Nothing
              }

      result <- AppointmentService.listAppointments app filters

      length result `shouldBe` 2

    it "usuário normal lista apenas seus agendamentos" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      AppointmentRepository.insertAppointment conn (request 1 1 3 10) "1234"
      AppointmentRepository.insertAppointment conn (request 2 2 3 11) "1234"

      let filters =
            Query.AppointmentQuery
              { Query.isAdmin = False
              , Query.customerId = Just 1
              , Query.date = Nothing
              , Query.machine = Nothing
              }

      result <- AppointmentService.listAppointments app filters

      map Appointment.customerId result `shouldBe` [1]

    it "admin filtra agendamentos por data" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      AppointmentRepository.insertAppointment conn (request 1 1 3 10) "1234"
      AppointmentRepository.insertAppointment conn (request 2 2 4 10) "1234"

      let filters =
            Query.AppointmentQuery
              { Query.isAdmin = True
              , Query.customerId = Nothing
              , Query.date = Just "2026-05-03"
              , Query.machine = Nothing
              }

      result <- AppointmentService.listAppointments app filters

      map Appointment.customerId result `shouldBe` [1]

    it "admin filtra agendamentos por cliente, data e máquina" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      AppointmentRepository.insertAppointment conn (request 1 1 3 10) "1234"
      AppointmentRepository.insertAppointment conn (request 1 2 3 11) "1234"
      AppointmentRepository.insertAppointment conn (request 2 1 3 10) "1234"

      let filters =
            Query.AppointmentQuery
              { Query.isAdmin = True
              , Query.customerId = Just 1
              , Query.date = Just "2026-05-03"
              , Query.machine = Just 2
              }

      result <- AppointmentService.listAppointments app filters

      map Appointment.customerId result `shouldBe` [1]
      map Appointment.machine result `shouldBe` [2]

  describe "AppointmentService deleteAppointment" $ do
    it "admin pode deletar qualquer agendamento" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      AppointmentRepository.insertAppointment conn (request 1 1 3 10) "1234"

      result <- AppointmentService.deleteAppointment app True 1 (Just 1)

      result `shouldBe` Right ()

    it "usuário pode deletar seu próprio agendamento" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      AppointmentRepository.insertAppointment conn (request 1 1 3 10) "1234"

      result <- AppointmentService.deleteAppointment app False 1 (Just 1)

      result `shouldBe` Right ()

    it "usuário não pode deletar agendamento de outro usuário" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      AppointmentRepository.insertAppointment conn (request 2 1 3 10) "1234"

      result <- AppointmentService.deleteAppointment app False 1 (Just 1)

      result `shouldBe` Left "Sem permissão para deletar"

    it "retorna erro quando agendamento não existe" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      result <- AppointmentService.deleteAppointment app False 999 (Just 1)

      result `shouldBe` Left "Agendamento não encontrado"

  describe "AppointmentService updateAppointmentStatus" $ do
    it "faz check-in quando a senha está correta" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      AppointmentRepository.insertAppointment conn (request 1 1 3 10) "1234"

      result <- AppointmentService.updateAppointmentStatus app 1 "1234" "checked_in"

      result `shouldBe` Right ()

      Just appointment <- AppointmentRepository.findAppointmentById conn 1
      Appointment.status appointment `shouldBe` "checked_in"

    it "finaliza agendamento quando a senha está correta" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      AppointmentRepository.insertAppointment conn (request 1 1 3 10) "1234"

      result <- AppointmentService.updateAppointmentStatus app 1 "1234" "finished"

      result `shouldBe` Right ()

      Just appointment <- AppointmentRepository.findAppointmentById conn 1
      Appointment.status appointment `shouldBe` "finished"

    it "não atualiza status quando a senha está errada" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      AppointmentRepository.insertAppointment conn (request 1 1 3 10) "1234"

      result <- AppointmentService.updateAppointmentStatus app 1 "9999" "checked_in"

      result `shouldBe` Left "Senha inválida"

    it "retorna erro quando agendamento não existe" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      result <- AppointmentService.updateAppointmentStatus app 999 "1234" "checked_in"

      result `shouldBe` Left "Agendamento não encontrado"