{-# LANGUAGE OverloadedStrings #-}

module Repositories.AppointmentRepository where

import Database.SQLite.Simple

import Models.Appointment

import Data.Time (LocalTime)

import Utils.DateTime (formatDbDateTime)

import qualified DTO.CreateAppointmentRequest as CreateReq

import qualified DTO.AppointmentQuery as Query

insertAppointment :: Connection -> CreateReq.CreateAppointmentRequest -> String -> IO ()
insertAppointment conn appointment generatedPassword =
  execute conn
    "INSERT INTO appointments (customer_id, machine, scheduled_at, password, status) VALUES (?, ?, ?, ?, ?)"
    ( CreateReq.customerId appointment
    , CreateReq.machine appointment
    , formatDbDateTime (CreateReq.scheduledAt appointment)
    , generatedPassword
    , "scheduled" :: String
    )

existsAppointmentAtMachineAndScheduledAt :: Connection -> Int -> LocalTime -> IO Bool
existsAppointmentAtMachineAndScheduledAt conn machineNumber scheduledAt = do
  result <- query conn
    "SELECT id FROM appointments \
    \WHERE machine = ? AND scheduled_at = ? \
    \AND status IN ('scheduled', 'checked_in') \
    \LIMIT 1"
    (machineNumber,  formatDbDateTime scheduledAt) :: IO [Only Int]

  pure $ not (null result)

findAppointments :: Connection -> Query.AppointmentQuery -> IO [Appointment]
findAppointments conn filters =
  query conn
    "SELECT id, customer_id, scheduled_at, machine, password, status \
    \FROM appointments \
    \WHERE (? IS NULL OR customer_id = ?) \
    \AND (? IS NULL OR date(scheduled_at) = ?) \
    \AND (? IS NULL OR machine = ?)"
    ( Query.customerId filters
    , Query.customerId filters
    , Query.date filters
    , Query.date filters
    , Query.machine filters
    , Query.machine filters
    )

findAppointmentById :: Connection -> Int -> IO (Maybe Appointment)
findAppointmentById conn appointmentId = do
  result <- query conn
    "SELECT id, customer_id, scheduled_at, machine, password, status FROM appointments WHERE id = ?"
    (Only appointmentId)

  pure $ case result of
    [a] -> Just a
    _   -> Nothing

findScheduledAppointmentsByScheduledAt :: Connection -> String -> IO [Appointment]
findScheduledAppointmentsByScheduledAt conn scheduledAt =
  query conn
    "SELECT id, customer_id, scheduled_at, machine, password, status \
    \FROM appointments \
    \WHERE scheduled_at = ? AND status = 'scheduled'"
    (Only scheduledAt)

deleteAppointmentById :: Connection -> Int -> IO ()
deleteAppointmentById conn appointmentId =
  execute conn
    "DELETE FROM appointments WHERE id = ?"
    (Only appointmentId)

updateAppointmentCustomer :: Connection -> Int -> Int -> IO ()
updateAppointmentCustomer conn appointmentId newCustomerId =
  execute conn
    "UPDATE appointments SET customer_id = ? WHERE id = ?"
    (newCustomerId, appointmentId)

updateAppointmentStatus :: Connection -> Int -> String -> IO ()
updateAppointmentStatus conn appointmentId newStatus =
  execute conn
    "UPDATE appointments SET status = ? WHERE id = ?"
    (newStatus, appointmentId)
