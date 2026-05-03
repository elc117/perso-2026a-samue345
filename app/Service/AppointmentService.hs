module Service.AppointmentService where

import App
import qualified Models.Appointment as Appointment
import qualified Repositories.AppointmentRepository as AppointmentRepository
import qualified DTO.CreateAppointmentRequest as CreateReq
import qualified DTO.CreateAppointmentResponse as CreateRes

createAppointment :: App -> CreateReq.CreateAppointmentRequest -> IO (Either String CreateRes.CreateAppointmentResponse)
createAppointment app appointment = do
  alreadyExists <-
    AppointmentRepository.existsAppointmentAtMachineAndScheduledAt
      (appDb app)
      (CreateReq.machine appointment)
      (CreateReq.scheduledAt appointment)

  if alreadyExists
    then pure $ Left "Essa máquina já está ocupada nesse horário."
    else do
      let generatedPassword = "1234"

      AppointmentRepository.insertAppointment (appDb app) appointment generatedPassword

      pure $ Right $
        CreateRes.CreateAppointmentResponse
          (CreateReq.machine appointment)
          (CreateReq.scheduledAt appointment)
          generatedPassword

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

updateAppointmentStatus :: App -> Int -> String -> String -> IO (Either String ())
updateAppointmentStatus app appointmentId inputPassword newStatus = do
  appointment <-
    AppointmentRepository.findAppointmentById
      (appDb app)
      appointmentId

  case appointment of
    Nothing ->
      pure $ Left "Agendamento não encontrado"

    Just appointment ->
      if Appointment.password appointment /= inputPassword
        then pure $ Left "Senha inválida"
        else do
          AppointmentRepository.updateAppointmentStatus
            (appDb app)
            appointmentId
            newStatus

          pure $ Right ()