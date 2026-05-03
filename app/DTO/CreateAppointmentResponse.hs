{-# LANGUAGE DeriveGeneric #-}

module DTO.CreateAppointmentResponse where

import Data.Aeson
import GHC.Generics

data CreateAppointmentResponse = CreateAppointmentResponse
  { 
    machine :: Int
  , scheduledAt :: String
  , password :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON CreateAppointmentResponse
instance ToJSON CreateAppointmentResponse