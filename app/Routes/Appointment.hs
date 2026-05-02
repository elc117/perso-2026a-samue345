{-# LANGUAGE OverloadedStrings #-}

module Routes.Appointment where

import Web.Scotty
import App
import Handler.AppointmentHandler

routes :: App -> ScottyM ()
routes app = do
  post "/appointments" $
    createAppointmentHandler app
  
  get "/appointments" $
    listAppointmentsHandler app