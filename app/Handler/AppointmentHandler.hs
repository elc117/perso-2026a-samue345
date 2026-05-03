{-# LANGUAGE OverloadedStrings #-}

module Handler.AppointmentHandler where

import Web.Scotty
import qualified Network.HTTP.Types.Status as HTTP
import Control.Monad.IO.Class

import App
import qualified Service.AppointmentService as AppointmentService
import DTO.CreateAppointmentRequest
import DTO.AppointmentQuery
import qualified DTO.CheckInRequest as CheckInReq

createAppointmentHandler :: App -> ActionM ()
createAppointmentHandler app = do
  appointment <- jsonData :: ActionM CreateAppointmentRequest

  result <- liftIO $
    AppointmentService.createAppointment app appointment

  case result of
    Left message -> do
      status HTTP.status409
      json message

    Right createdAppointment -> do
      status HTTP.status201
      json createdAppointment

listAppointmentsHandler :: App -> ActionM ()
listAppointmentsHandler app = do
  role <- queryParam "role"
  customerId <- queryParamMaybe "customer_id"
  date <- queryParamMaybe "date"
  machine <- queryParamMaybe "machine"

  let filters =
        AppointmentQuery
          { isAdmin = role == ("admin" :: String)
          , customerId = customerId
          , date = date
          , machine = machine
          }

  appointments <- liftIO $
    AppointmentService.listAppointments app filters

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
      status HTTP.status403
      json err

    Right _ -> do
      status HTTP.status204

checkInAppointmentHandler :: App -> ActionM ()
checkInAppointmentHandler app = do
  appointmentId <- captureParam "id"
  req <- jsonData :: ActionM CheckInReq.CheckInRequest

  result <- liftIO $
    AppointmentService.updateAppointmentStatus
      app
      appointmentId
      (CheckInReq.password req)
      "checked_in"

  handleStatusResult result

finishAppointmentHandler :: App -> ActionM ()
finishAppointmentHandler app = do
  appointmentId <- captureParam "id"
  req <- jsonData :: ActionM CheckInReq.CheckInRequest

  result <- liftIO $
    AppointmentService.updateAppointmentStatus
      app
      appointmentId
      (CheckInReq.password req)
      "finished"

  handleStatusResult result


handleStatusResult :: Either String () -> ActionM ()
handleStatusResult result =
  case result of
    Left err -> do
      status HTTP.status400
      json err

    Right _ ->
     status HTTP.status204