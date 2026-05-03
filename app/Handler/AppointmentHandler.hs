{-# LANGUAGE OverloadedStrings #-}

module Handler.AppointmentHandler where

import Web.Scotty
import Network.HTTP.Types.Status
import Control.Monad.IO.Class

import App
import Models.Appointment
import qualified Service.AppointmentService as AppointmentService
import DTO.CreateAppointmentRequest


createAppointmentHandler :: App -> ActionM ()
createAppointmentHandler app = do
  appointment <- jsonData :: ActionM CreateAppointmentRequest

  result <- liftIO $
    AppointmentService.createAppointment app appointment

  case result of
    Left message -> do
      status status409
      json message

    Right createdAppointment -> do
      status status201
      json createdAppointment

listAppointmentsHandler :: App -> ActionM ()
listAppointmentsHandler app = do
  role <- queryParam "role"
  customerId <- queryParamMaybe "customer_id"

  let isAdmin = role == ("admin" :: String)

  appointments <- liftIO $
    AppointmentService.listAppointments app isAdmin customerId

  json appointments

deleteAppointmentHandler :: App -> ActionM ()
deleteAppointmentHandler app = do
  appointmentId <- captureParam "id"

  role <- queryParam "role"
  requesterId <- queryParamMaybe "customer_id"


  let isAdmin = role == ("admin" :: String)

  result <- liftIO $
    AppointmentService.deleteAppointment app isAdmin appointmentId requesterId

  case result of
    Left err -> do
      status status403
      json err

    Right _ -> do
      status status204