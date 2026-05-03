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

  describe "AppointmentService listAppointments" $ do
    it "admin lista todos os agendamentos" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      AppointmentRepository.insertAppointment conn
        CreateAppointmentRequest { customerId = 1, scheduledAt = scheduled 10, machine = 1 }
        "1234"

      AppointmentRepository.insertAppointment conn
        CreateAppointmentRequest { customerId = 2, scheduledAt = scheduled 11, machine = 2 }
        "1234"

      result <- AppointmentService.listAppointments app True Nothing

      length result `shouldBe` 2

    it "usuário normal lista apenas seus agendamentos" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      AppointmentRepository.insertAppointment conn
        CreateAppointmentRequest { customerId = 1, scheduledAt = scheduled 10, machine = 1 }
        "1234"

      AppointmentRepository.insertAppointment conn
        CreateAppointmentRequest { customerId = 2, scheduledAt = scheduled 11, machine = 2 }
        "1234"

      result <- AppointmentService.listAppointments app False (Just 1)

      length result `shouldBe` 1
      customerId (head result) `shouldBe` 1

  describe "AppointmentService deleteAppointment" $ do
    it "admin pode deletar qualquer agendamento" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      AppointmentRepository.insertAppointment conn
        CreateAppointmentRequest { customerId = 1, scheduledAt = scheduled 10, machine = 1 }
        "1234"

      result <- AppointmentService.deleteAppointment app True 1 (Just 1)

      result `shouldBe` Right ()

    it "usuário pode deletar seu próprio agendamento" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      AppointmentRepository.insertAppointment conn
        CreateAppointmentRequest { customerId = 1, scheduledAt = scheduled 10, machine = 1 }
        "1234"

      result <- AppointmentService.deleteAppointment app False 1 (Just 1)

      result `shouldBe` Right ()

    it "usuário não pode deletar agendamento de outro usuário" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      AppointmentRepository.insertAppointment conn
        CreateAppointmentRequest { customerId = 2, scheduledAt = scheduled 10, machine = 1 }
        "1234"

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

    AppointmentRepository.insertAppointment conn
      CreateAppointmentRequest { customerId = 1, scheduledAt = scheduled 10, machine = 1 }
      "1234"

    result <- AppointmentService.updateAppointmentStatus app 1 "1234" "checked_in"

    result `shouldBe` Right ()

    Just appointment <- AppointmentRepository.findAppointmentById conn 1
    status appointment `shouldBe` "checked_in"

  it "finaliza agendamento quando a senha está correta" $ do
    conn <- open ":memory:"
    createTables conn

    let app = App { appDb = conn }

    AppointmentRepository.insertAppointment conn
      CreateAppointmentRequest { customerId = 1, scheduledAt = scheduled 10, machine = 1 }
      "1234"

    result <- AppointmentService.updateAppointmentStatus app 1 "1234" "finished"

    result `shouldBe` Right ()

    Just appointment <- AppointmentRepository.findAppointmentById conn 1
    status appointment `shouldBe` "finished"

  it "não atualiza status quando a senha está errada" $ do
    conn <- open ":memory:"
    createTables conn

    let app = App { appDb = conn }

    AppointmentRepository.insertAppointment conn
      CreateAppointmentRequest { customerId = 1, scheduledAt = scheduled 10, machine = 1 }
      "1234"

    result <- AppointmentService.updateAppointmentStatus app 1 "9999" "checked_in"

    result `shouldBe` Left "Senha inválida"

  it "retorna erro quando agendamento não existe" $ do
    conn <- open ":memory:"
    createTables conn

    let app = App { appDb = conn }

    result <- AppointmentService.updateAppointmentStatus app 999 "1234" "checked_in"

    result `shouldBe` Left "Agendamento não encontrado"