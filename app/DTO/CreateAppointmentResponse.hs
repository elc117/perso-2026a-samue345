{-# LANGUAGE DeriveGeneric #-}

module DTO.CreateAppointmentResponse where

import Data.Aeson
import Data.Time
import GHC.Generics

data CreateAppointmentResponse = CreateAppointmentResponse
  { 
    machine :: Int
  , scheduledAt :: LocalTime
  , password :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON CreateAppointmentResponse
instance ToJSON CreateAppointmentResponse
