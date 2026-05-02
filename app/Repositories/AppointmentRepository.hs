{-# LANGUAGE OverloadedStrings #-}

module Repositories.AppointmentRepository where

import Database.SQLite.Simple

import Models.Appointment

insertAppointment :: Connection -> Appointment -> IO ()
insertAppointment conn appointment =
  execute conn
    "INSERT INTO appointments (customer_id, machine, time) VALUES (?, ?, ?)"
    (customerId appointment, machine appointment, time appointment)

existsAppointmentAtMachineAndTime :: Connection -> Int -> String -> IO Bool
existsAppointmentAtMachineAndTime conn machineNumber appointmentTime = do
  result <- query conn
    "SELECT id FROM appointments WHERE machine = ? AND time = ? LIMIT 1"
    (machineNumber, appointmentTime) :: IO [Only Int]

  pure $ not (null result)

findAllAppointments :: Connection -> IO [Appointment]
findAllAppointments conn =
  query_ conn
    "SELECT customer_id, time, machine FROM appointments"

findAppointmentsByCustomer :: Connection -> Int -> IO [Appointment]
findAppointmentsByCustomer conn customerId =
  query conn
    "SELECT customer_id, time, machine FROM appointments WHERE customer_id = ?"
    (Only customerId)

findAppointmentById :: Connection -> Int -> IO (Maybe Appointment)
findAppointmentById conn appointmentId = do
  result <- query conn
    "SELECT customer_id, time, machine FROM appointments WHERE id = ?"
    (Only appointmentId)

  pure $ case result of
    [a] -> Just a
    _   -> Nothing