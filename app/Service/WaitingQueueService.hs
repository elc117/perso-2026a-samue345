module Service.WaitingQueueService where

import App
import Data.List (find)
import Data.Time
import Data.Time.Format

import qualified Models.WaitingQueue as Queue
import qualified Models.Appointment as Appointment

import qualified Repositories.WaitingQueueRepository as WaitingQueueRepository
import qualified Repositories.AppointmentRepository as AppointmentRepository
import Utils.DateTime (minutesFromTimeOfDay, stringToTimeOfDay)

type TimeRange = (String, String)

appointmentSlots :: [TimeRange]
appointmentSlots =
  [ ("08:00", "09:08")
  , ("09:08", "10:08")
  , ("10:08", "11:08")
  , ("11:08", "12:08")
  , ("12:08", "13:08")
  , ("13:08", "14:08")
  , ("14:08", "15:08")
  , ("15:08", "16:08")
  , ("16:08", "17:08")
  ]

delayToleranceMinutes :: Int
delayToleranceMinutes = 8

joinQueue :: App -> Queue.WaitingQueue -> IO ()
joinQueue app queue =
  WaitingQueueRepository.insertQueue (appDb app) queue

checkDelayAndReleaseQueue :: App -> IO ()
checkDelayAndReleaseQueue app = do
  now <- getZonedTime
  checkDelayAndReleaseQueueAt app now

checkDelayAndReleaseQueueAt :: App -> ZonedTime -> IO ()
checkDelayAndReleaseQueueAt app now = do
  let currentMinute = minutesFromTimeOfDay $
        localTimeOfDay (zonedTimeToLocalTime now)

      today =
        localDay (zonedTimeToLocalTime now)

  case findCurrentSlot currentMinute of
    Nothing ->
      pure ()

    Just (startTime, _) -> do
      let scheduledAt = buildScheduledAt today startTime

      delayedAppointments <-
        AppointmentRepository.findScheduledAppointmentsByScheduledAt
          (appDb app)
          scheduledAt

      queue <-
        WaitingQueueRepository.getQueueByScheduledAt
          (appDb app)
          scheduledAt

      let replacements = zip delayedAppointments queue

      mapM_ replaceAppointmentCustomer replacements

  where
    replaceAppointmentCustomer (appointment, queueItem) = do
      AppointmentRepository.updateAppointmentCustomer
        (appDb app)
        (Appointment.appointmentId appointment)
        (Queue.customerId queueItem)

      WaitingQueueRepository.deleteQueueByCustomerAndScheduledAt
        (appDb app)
        (Queue.customerId queueItem)
        (Appointment.scheduledAt appointment)

buildScheduledAt :: Day -> String -> String
buildScheduledAt day timeValue =
  formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" localTime
  where
    localTime =
      LocalTime day (stringToTimeOfDay timeValue)

findCurrentSlot :: Int -> Maybe TimeRange
findCurrentSlot currentMinute =
  find isInside appointmentSlots
  where
    isInside (startTime, endTime) =
      let start = minutesFromTimeOfDay (stringToTimeOfDay startTime)
          end = minutesFromTimeOfDay (stringToTimeOfDay endTime)
          releaseAfter = start + delayToleranceMinutes
       in currentMinute >= releaseAfter && currentMinute < end
