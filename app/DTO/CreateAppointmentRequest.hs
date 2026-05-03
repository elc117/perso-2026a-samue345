{-# LANGUAGE DeriveGeneric #-}

module DTO.CreateAppointmentRequest where

import Data.Aeson
import GHC.Generics

data CreateAppointmentRequest = CreateAppointmentRequest
  { customerId :: Int
  , time :: String
  , machine :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON CreateAppointmentRequest
instance ToJSON CreateAppointmentRequest