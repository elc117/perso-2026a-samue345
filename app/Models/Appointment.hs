{-# Language DeriveGeneric #-}

module Models.Appointment where

import Data.Aeson
import GHC.Generics


data Appointment = Appointment { 
  customer :: String, 
  time :: String,
  machine :: Int
} deriving (Show, Generic)

instance ToJSON Appointment
instance FromJSON Appointment