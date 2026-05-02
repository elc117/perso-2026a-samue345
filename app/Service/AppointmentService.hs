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

listAppointments :: App -> Bool -> Int -> IO [Appointment]
listAppointments app isAdmin customerId =
  if isAdmin
    then AppointmentRepository.findAllAppointments (appDb app)
    else AppointmentRepository.findAppointmentsByCustomerId (appDb app) customerId


deleteAppointment :: App -> Bool -> Int -> Int -> IO (Either String ())
deleteAppointment app isAdmin requesterId appointmentId = do
  maybeAppointment <-
    AppointmentRepository.findAppointmentById
      (appDb app)
      appointmentId

  case maybeAppointment of
    Nothing ->
      pure $ Left "Agendamento não encontrado"

    Just appointment ->
      if isAdmin || customerId appointment == requesterId
        then do
          AppointmentRepository.deleteAppointmentById
            (appDb app)
            appointmentId

          pure $ Right ()
        else
          pure $ Left "Sem permissão para deletar"