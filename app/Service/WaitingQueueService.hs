module Service.WaitingQueueService where

import App

import qualified Repositories.WaitingQueueRepository as WaitingQueueRepository
import qualified Repositories.AppointmentRepository as AppointmentRepository

import qualified Models.WaitingQueue as Queue
import qualified Models.Appointment as Appointment
import Data.Time


joinQueue :: App -> Queue.WaitingQueue -> IO ()
joinQueue app queue =
  WaitingQueueRepository.insertQueue (appDb app) queue

checkDelayAndReleaseQueue :: App -> String -> IO ()
checkDelayAndReleaseQueue app appointmentTime = do
  now <- getZonedTime

  let currentMinute = minutesFromTime (Right now)
      scheduledMinute = minutesFromTime (Left appointmentTime)

  if currentMinute < scheduledMinute + 8
    then pure ()
    else do
      delayedAppointments <-
        AppointmentRepository.findScheduledAppointmentsByTime
          (appDb app)
          appointmentTime

      queue <-
        WaitingQueueRepository.getQueueByTime
          (appDb app)
          appointmentTime

      let replacements = zip delayedAppointments queue

      mapM_ replaceAppointmentCustomer replacements

  where
    replaceAppointmentCustomer (appointment, queueItem) = do
      AppointmentRepository.updateAppointmentCustomer
        (appDb app)
        (Appointment.appointmentId appointment)
        (Queue.customerId queueItem)

      WaitingQueueRepository.deleteQueueByCustomerAndTime
        (appDb app)
        (Queue.customerId queueItem)
        appointmentTime
        
minutesFromTime :: Either String ZonedTime -> Int
minutesFromTime input =
  case input of
    Left value ->
      toMinutes (stringToTimeOfDay value)

    Right zonedTime ->
      toMinutes (localTimeOfDay (zonedTimeToLocalTime zonedTime))


toMinutes :: TimeOfDay -> Int
toMinutes t =
  todHour t * 60 + todMin t

stringToTimeOfDay :: String -> TimeOfDay
stringToTimeOfDay value =
  let hour = read (take 2 value) :: Int
      minute = read (drop 3 value) :: Int
   in TimeOfDay hour minute 0