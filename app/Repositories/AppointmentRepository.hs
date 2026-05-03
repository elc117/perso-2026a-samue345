{-# LANGUAGE OverloadedStrings #-}

module Repositories.AppointmentRepository where

import Database.SQLite.Simple

import Models.Appointment

import qualified DTO.CreateAppointmentRequest as CreateReq

insertAppointment :: Connection -> CreateReq.CreateAppointmentRequest -> String -> IO ()
insertAppointment conn appointment generatedPassword =
  execute conn
    "INSERT INTO appointments (customer_id, machine, scheduledAt, password, status) VALUES (?, ?, ?, ?, ?)"
    ( CreateReq.customerId appointment
    , CreateReq.machine appointment
    , CreateReq.scheduledAt appointment
    , generatedPassword
    , "scheduled" :: String
    )

existsAppointmentAtMachineAndTime :: Connection -> Int -> String -> IO Bool
existsAppointmentAtMachineAndTime conn machineNumber appointmentTime = do
  result <- query conn
    "SELECT id FROM appointments \
    \WHERE machine = ? AND scheduled_at = ? \
    \AND status IN ('scheduled', 'checked_in') \
    \LIMIT 1"
    (machineNumber, appointmentTime) :: IO [Only Int]

  pure $ not (null result)

findAllAppointments :: Connection -> IO [Appointment]
findAllAppointments conn =
  query_ conn
    "SELECT id, customer_id, scheduled_at, machine, password, status FROM appointments"

findAppointmentsByCustomer :: Connection -> Int -> IO [Appointment]
findAppointmentsByCustomer conn customerId =
  query conn
    "SELECT id, customer_id, scheduled_at, machine, password, status FROM appointments WHERE customer_id = ?"
    (Only customerId)

findAppointmentById :: Connection -> Int -> IO (Maybe Appointment)
findAppointmentById conn appointmentId = do
  result <- query conn
    "SELECT id, customer_id, scheduled_at, machine, password, status FROM appointments WHERE id = ?"
    (Only appointmentId)

  pure $ case result of
    [a] -> Just a
    _   -> Nothing

findScheduledAppointmentsByTime :: Connection -> String -> IO [Appointment]
findScheduledAppointmentsByTime conn appointmentTime =
  query conn
    "SELECT id, customer_id, scheduled_at, machine, password, status \
    \FROM appointments \
    \WHERE scheduled_at = ? AND status = 'scheduled'"
    (Only appointmentTime)

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