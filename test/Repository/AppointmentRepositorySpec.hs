{-# LANGUAGE OverloadedStrings #-}

module Service.AppointmentServiceSpec (spec) where

import Test.Hspec
import Database.SQLite.Simple

import App
import Database.Connection
import Models.Appointment
import DTO.CreateAppointmentRequest

import qualified Repositories.AppointmentRepository as AppointmentRepository
import qualified Service.AppointmentService as AppointmentService

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
            , time = "10:00"
            }

      result <- AppointmentService.createAppointment app request

      result `shouldBe` Right request

    it "bloqueia agendamento duplicado na mesma máquina e horário" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      let request = CreateAppointmentRequest
            { customerId = 1
            , machine = 1
            , time = "10:00"
            }

      _ <- AppointmentService.createAppointment app request
      result <- AppointmentService.createAppointment app request

      result `shouldBe` Left "Essa máquina já está ocupada nesse horário."

  describe "AppointmentService listAppointments" $ do
    it "admin lista todos os agendamentos" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      AppointmentRepository.insertAppointment conn $
        CreateAppointmentRequest { customerId = 1, time = "10:00", machine = 1 }

      AppointmentRepository.insertAppointment conn $
        CreateAppointmentRequest { customerId = 2, time = "11:00", machine = 2 }

      result <- AppointmentService.listAppointments app True 0

      result `shouldBe`
        [ Appointment { appointmentId = 1, customerId = 1, time = "10:00", machine = 1 }
        , Appointment { appointmentId = 2, customerId = 2, time = "11:00", machine = 2 }
        ]

    it "usuário normal lista apenas seus agendamentos" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      AppointmentRepository.insertAppointment conn $
        CreateAppointmentRequest { customerId = 1, time = "10:00", machine = 1 }

      AppointmentRepository.insertAppointment conn $
        CreateAppointmentRequest { customerId = 2, time = "11:00", machine = 2 }

      result <- AppointmentService.listAppointments app False 1

      result `shouldBe`
        [ Appointment { appointmentId = 1, customerId = 1, time = "10:00", machine = 1 }
        ]

  describe "AppointmentService deleteAppointment" $ do
    it "admin pode deletar qualquer agendamento" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      AppointmentRepository.insertAppointment conn $
        CreateAppointmentRequest { customerId = 1, time = "10:00", machine = 1 }

      result <- AppointmentService.deleteAppointment app True 999 1

      result `shouldBe` Right ()

    it "usuário pode deletar seu próprio agendamento" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      AppointmentRepository.insertAppointment conn $
        CreateAppointmentRequest { customerId = 1, time = "10:00", machine = 1 }

      result <- AppointmentService.deleteAppointment app False 1 1

      result `shouldBe` Right ()

    it "usuário não pode deletar agendamento de outro usuário" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      AppointmentRepository.insertAppointment conn $
        CreateAppointmentRequest { customerId = 1, time = "10:00", machine = 1 }

      result <- AppointmentService.deleteAppointment app False 2 1

      result `shouldBe` Left "Sem permissão para deletar"

    it "retorna erro quando agendamento não existe" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      result <- AppointmentService.deleteAppointment app False 1 999

      result `shouldBe` Left "Agendamento não encontrado"