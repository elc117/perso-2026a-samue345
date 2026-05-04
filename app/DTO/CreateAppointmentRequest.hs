{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module DTO.CreateAppointmentRequest where

import Data.Aeson
import Data.Time
import Data.Time.Format
import GHC.Generics

data CreateAppointmentRequest = CreateAppointmentRequest
  { customerId  :: Int
  , scheduledAt :: LocalTime
  , machine     :: Int
  }
  deriving (Show, Eq, Generic)
instance FromJSON CreateAppointmentRequest where
  parseJSON = withObject "CreateAppointmentRequest" $ \v -> do
    customerId <- v .: "customerId"
    machine <- v .: "machine"

    scheduledAt <-
      v .: "scheduledAt" >>= \str ->
        case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" str of
          Just t  -> pure t
          Nothing -> fail "scheduledAt deve estar no formato YYYY-MM-DD HH:MM:SS"

    pure CreateAppointmentRequest
      { customerId = customerId
      , machine = machine
      , scheduledAt = scheduledAt
      }

instance ToJSON CreateAppointmentRequest
