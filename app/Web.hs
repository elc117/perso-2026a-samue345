module Web where

import Web.Scotty
import App
import qualified Routes.Appointment as AppointmentRoutes

routes :: App -> ScottyM ()
routes app = do
  AppointmentRoutes.routes app