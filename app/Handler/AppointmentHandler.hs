module Handler.AppointmentHandler where

import Web.Scotty
import Network.HTTP.Types.Status
import Control.Monad.IO.Class

import App
import Models.Appointment
import qualified Service.AppointmentService as AppointmentService

createAppointmentHandler :: App -> ActionM ()
createAppointmentHandler app = do
  appointment <- jsonData :: ActionM Appointment

  result <- liftIO $
    AppointmentService.createAppointment app appointment

  case result of
    Left message -> do
      status status409
      json message

    Right createdAppointment -> do
      status status201
      json createdAppointment