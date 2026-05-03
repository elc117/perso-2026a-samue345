module Service.WaitingQueueService where

import App
import Data.List (find)
import Data.Time

import qualified Models.WaitingQueue as Queue
import qualified Models.Appointment as Appointment

import qualified Repositories.WaitingQueueRepository as WaitingQueueRepository
import qualified Repositories.AppointmentRepository as AppointmentRepository

type TimeRange = (String, String)

appointmentSlots :: [TimeRange]
appointmentSlots =
  [ ("08:00", "09:00")
  , ("09:00", "10:00")
  , ("10:00", "11:00")
  , ("11:00", "12:00")
  , ("12:00", "13:00")
  , ("13:00", "14:00")
  , ("14:00", "15:00")
  , ("15:00", "16:00")
  , ("16:00", "17:00")
  ]

delayToleranceMinutes :: Int
delayToleranceMinutes = 8

joinQueue :: App -> Queue.WaitingQueue -> IO ()
joinQueue app queue =
  WaitingQueueRepository.insertQueue (appDb app) queue

checkDelayAndReleaseQueue :: App -> IO ()
checkDelayAndReleaseQueue app = do
  now <- getZonedTime

  let currentMinute = minutesFromTime (Right now)

  case findCurrentSlot currentMinute of
    Nothing ->
      pure ()

    Just (startTime, _) -> do
      delayedAppointments <-
        AppointmentRepository.findScheduledAppointmentsByTime
          (appDb app)
          startTime

      queue <-
        WaitingQueueRepository.getQueueByTime
          (appDb app)
          startTime

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
        (Appointment.scheduledAt appointment)

findCurrentSlot :: Int -> Maybe TimeRange
findCurrentSlot currentMinute =
  find isInside appointmentSlots
  where
    isInside (startTime, endTime) =
      let start = minutesFromTime (Left startTime)
          end = minutesFromTime (Left endTime)
          releaseAfter = start + delayToleranceMinutes
       in currentMinute >= releaseAfter && currentMinute < end

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