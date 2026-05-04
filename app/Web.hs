module Web where

import Web.Scotty
import App
import qualified Routes.Appointment as AppointmentRoutes
import qualified Routes.WaitingQueue as WaitingQueueRoutes


routes :: App -> ScottyM ()
routes app = do
  AppointmentRoutes.routes app
  WaitingQueueRoutes.routes app
  