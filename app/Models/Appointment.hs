{-# Language DeriveGeneric #-}

module Models.Appointment where

import Data.Aeson
import GHC.Generics


data Appointment = Appointment { 
  customer :: String, 
  time :: String,
  machine :: Int
} deriving (Show, Eq, Generic)

instance ToJSON Appointment
instance FromJSON Appointment