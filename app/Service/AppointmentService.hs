module Service.AppointmentService where

import App
import Models.Appointment
import qualified Repositories.AppointmentRepository as AppointmentRepository

createAppointment :: App -> Appointment -> IO (Either String Appointment)
createAppointment app appointment = do
  alreadyExists <-
    AppointmentRepository.existsAppointmentAtMachineAndTime
      (appDb app)
      (machine appointment)
      (time appointment)

  if alreadyExists
    then pure $ Left "Essa máquina já está ocupada nesse horário."
    else do
      AppointmentRepository.insertAppointment
        (appDb app)
        appointment

      pure $ Right appointment