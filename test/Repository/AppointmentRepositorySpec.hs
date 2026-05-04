{-# LANGUAGE OverloadedStrings #-}

module Repository.AppointmentRepositorySpec (spec) where

import Test.Hspec
import Database.SQLite.Simple
import Data.Time

import Database.Connection

import qualified Models.Appointment as Appo
import qualified DTO.CreateAppointmentRequest as Req
import qualified DTO.AppointmentQuery as Query

import qualified Repositories.AppointmentRepository as AppointmentRepository

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
  describe "AppointmentRepository" $ do
    it "insere e encontra agendamento por máquina e data/hora" $ do
      conn <- open ":memory:"
      createTables conn

      AppointmentRepository.insertAppointment conn (request 1 1 3 10) "1234"

      exists <- AppointmentRepository.existsAppointmentAtMachineAndScheduledAt conn 1 (scheduled 3 10)

      exists `shouldBe` True

    it "retorna False quando não existe agendamento" $ do
      conn <- open ":memory:"
      createTables conn

      exists <- AppointmentRepository.existsAppointmentAtMachineAndScheduledAt conn 1 (scheduled 3 10)

      exists `shouldBe` False

    it "não considera conflito no mesmo horário em outro dia" $ do
      conn <- open ":memory:"
      createTables conn

      AppointmentRepository.insertAppointment conn (request 1 1 3 10) "1234"

      exists <- AppointmentRepository.existsAppointmentAtMachineAndScheduledAt conn 1 (scheduled 4 10)

      exists `shouldBe` False

    it "não considera conflito em outro horário no mesmo dia" $ do
      conn <- open ":memory:"
      createTables conn

      AppointmentRepository.insertAppointment conn (request 1 1 3 10) "1234"

      exists <- AppointmentRepository.existsAppointmentAtMachineAndScheduledAt conn 1 (scheduled 3 11)

      exists `shouldBe` False

    it "busca agendamento por id" $ do
      conn <- open ":memory:"
      createTables conn

      AppointmentRepository.insertAppointment conn (request 1 1 3 10) "1234"

      result <- AppointmentRepository.findAppointmentById conn 1

      result `shouldBe`
        Just Appo.Appointment
          { Appo.appointmentId = 1
          , Appo.customerId = 1
          , Appo.scheduledAt = "2026-05-03 10:00:00"
          , Appo.machine = 1
          , Appo.password = "1234"
          , Appo.status = "scheduled"
          }

    it "retorna Nothing quando id não existe" $ do
      conn <- open ":memory:"
      createTables conn

      result <- AppointmentRepository.findAppointmentById conn 999

      result `shouldBe` Nothing

    it "lista todos quando não há filtros" $ do
      conn <- open ":memory:"
      createTables conn

      AppointmentRepository.insertAppointment conn (request 1 1 3 10) "1234"
      AppointmentRepository.insertAppointment conn (request 2 2 4 11) "1234"

      result <- AppointmentRepository.findAppointments conn
        Query.AppointmentQuery
          {Query.isAdmin = True
          , Query.customerId = Nothing
          , Query.date = Nothing
          , Query.machine = Nothing
          }

      length result `shouldBe` 2

    it "filtra por customerId" $ do
      conn <- open ":memory:"
      createTables conn

      AppointmentRepository.insertAppointment conn (request 1 1 3 10) "1234"
      AppointmentRepository.insertAppointment conn (request 2 2 3 11) "1234"

      result <- AppointmentRepository.findAppointments conn
         Query.AppointmentQuery
          { Query.isAdmin = True
          , Query.customerId = Nothing
          , Query.date = Nothing
          , Query.machine = Just 2
          }

      map Appo.customerId result `shouldBe` [2]

    it "filtra por data" $ do
      conn <- open ":memory:"
      createTables conn

      AppointmentRepository.insertAppointment conn (request 1 1 3 10) "1234"
      AppointmentRepository.insertAppointment conn (request 2 2 4 10) "1234"

      result <- AppointmentRepository.findAppointments conn
        Query.AppointmentQuery
          { Query.isAdmin = True
          , Query.customerId = Nothing
          , Query.date = Just "2026-05-03"
          , Query.machine = Nothing
          }

      map Appo.customerId result `shouldBe` [1]

    it "filtra por máquina" $ do
      conn <- open ":memory:"
      createTables conn

      AppointmentRepository.insertAppointment conn (request 1 1 3 10) "1234"
      AppointmentRepository.insertAppointment conn (request 2 2 3 11) "1234"

      result <- AppointmentRepository.findAppointments conn
        Query.AppointmentQuery
          { Query.isAdmin = True
          , Query.customerId = Nothing
          , Query.date = Nothing
          , Query.machine = Just 2
          }

      map Appo.machine result `shouldBe` [2]

    it "filtra por customerId, data e máquina juntos" $ do
      conn <- open ":memory:"
      createTables conn

      AppointmentRepository.insertAppointment conn (request 1 1 3 10) "1234"
      AppointmentRepository.insertAppointment conn (request 1 2 3 11) "1234"
      AppointmentRepository.insertAppointment conn (request 2 2 3 11) "1234"

      result <- AppointmentRepository.findAppointments conn
        Query.AppointmentQuery
          { Query.isAdmin = True
          , Query.customerId = Just 1
          , Query.date = Just "2026-05-03"
          , Query.machine = Just 2
          }

      length result `shouldBe` 1
      Appo.customerId (head result) `shouldBe` 1
      Appo.machine (head result) `shouldBe` 2

    it "busca agendamentos scheduled por scheduled_at" $ do
      conn <- open ":memory:"
      createTables conn

      AppointmentRepository.insertAppointment conn (request 1 1 3 10) "1234"

      result <- AppointmentRepository.findScheduledAppointmentsByScheduledAt conn "2026-05-03 10:00:00"

      length result `shouldBe` 1
      Appo.customerId (head result) `shouldBe` 1

    it "atualiza cliente do agendamento" $ do
      conn <- open ":memory:"
      createTables conn

      AppointmentRepository.insertAppointment conn (request 1 1 3 10) "1234"

      AppointmentRepository.updateAppointmentCustomer conn 1 9

      Just appointment <- AppointmentRepository.findAppointmentById conn 1

      Appo.customerId appointment `shouldBe` 9

    it "atualiza status do agendamento" $ do
      conn <- open ":memory:"
      createTables conn

      AppointmentRepository.insertAppointment conn (request 1 1 3 10) "1234"

      AppointmentRepository.updateAppointmentStatus conn 1 "checked_in"

      Just appointment <- AppointmentRepository.findAppointmentById conn 1

      Appo.status appointment `shouldBe` "checked_in"

    it "deleta agendamento por id" $ do
      conn <- open ":memory:"
      createTables conn

      AppointmentRepository.insertAppointment conn (request 1 1 3 10) "1234"
      AppointmentRepository.deleteAppointmentById conn 1

      result <- AppointmentRepository.findAppointmentById conn 1

      result `shouldBe` Nothing
