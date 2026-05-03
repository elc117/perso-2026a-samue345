module Service.WaitingQueueService where

import App

import qualified Repositories.WaitingQueueRepository as WaitingQueueRepository
import qualified Repositories.AppointmentRepository as AppointmentRepository

import qualified Models.WaitingQueue as Queue
import qualified Models.Appointment as Appointment

joinQueue :: App -> Queue.WaitingQueue -> IO ()
joinQueue app queue =
  WaitingQueueRepository.insertQueue (appDb app) queue

checkDelayAndReleaseQueue :: App -> String -> Int -> IO ()
checkDelayAndReleaseQueue app appointmentTime currentMinute = do
  let scheduledMinute = timeToMinutes appointmentTime

  if currentMinute < scheduledMinute + 8
    then pure ()
    else do
      delayedAppointments <-
        AppointmentRepository.findAppointmentsByTime
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
        
timeToMinutes :: String -> Int
timeToMinutes value =
  let hour = read (take 2 value) :: Int
      minute = read (drop 3 value) :: Int
   in hour * 60 + minute