{-# LANGUAGE OverloadedStrings #-}

module Repositories.AppointmentRepository where

import Database.SQLite.Simple

import Models.Appointment

insertAppointment :: Connection -> Appointment -> IO ()
insertAppointment conn appointment =
  execute conn
    "INSERT INTO appointments (customer, machine, time) VALUES (?, ?, ?)"
    (customer appointment, machine appointment, time appointment)

existsAppointmentAtMachineAndTime :: Connection -> Int -> String -> IO Bool
existsAppointmentAtMachineAndTime conn machineNumber appointmentTime = do
  result <- query conn
    "SELECT id FROM appointments WHERE machine = ? AND time = ? LIMIT 1"
    (machineNumber, appointmentTime) :: IO [Only Int]

  pure $ not (null result)