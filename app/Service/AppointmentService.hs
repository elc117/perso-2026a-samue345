module Service.AppointmentService where

import App
import qualified Models.Appointment as Appointment
import qualified Repositories.AppointmentRepository as AppointmentRepository
import qualified DTO.CreateAppointmentRequest as CreateReq

createAppointment :: App -> CreateReq.CreateAppointmentRequest -> IO (Either String CreateReq.CreateAppointmentRequest)
createAppointment app appointment = do
  alreadyExists <-
    AppointmentRepository.existsAppointmentAtMachineAndTime
      (appDb app)
      (CreateReq.machine appointment)
      (CreateReq.time appointment)

  if alreadyExists
    then pure $ Left "Essa máquina já está ocupada nesse horário."
    else do
      AppointmentRepository.insertAppointment (appDb app) appointment
      pure $ Right appointment

listAppointments :: App -> Bool -> Maybe Int -> IO [Appointment.Appointment]
listAppointments app isAdmin customerId =
  if isAdmin
    then AppointmentRepository.findAllAppointments (appDb app)
    else case customerId of
      Just cid ->
        AppointmentRepository.findAppointmentsByCustomer (appDb app) cid

      Nothing ->
        pure []

deleteAppointment :: App -> Bool -> Int -> Maybe Int -> IO (Either String ())
deleteAppointment app isAdmin appointmentId requesterId = do
  appointments <-
    AppointmentRepository.findAppointmentById
      (appDb app)
      appointmentId

  case appointments of
    Nothing ->
      pure $ Left "Agendamento não encontrado"

    Just appointment ->
      if isAdmin || Just (Appointment.customerId appointment) == requesterId
        then do
          AppointmentRepository.deleteAppointmentById
            (appDb app)
            appointmentId

          pure $ Right ()
        else
          pure $ Left "Sem permissão para deletar"