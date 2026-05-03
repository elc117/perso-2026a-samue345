{-# LANGUAGE DeriveGeneric #-}

module Models.Appointment where

import Data.Aeson
import GHC.Generics
import Database.SQLite.Simple

data Appointment = Appointment
  { appointmentId :: Int
  , customerId :: Int
  , time :: String
  , machine :: Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON Appointment
instance FromJSON Appointment

instance FromRow Appointment where
  fromRow = Appointment <$> field <*> field <*> field <*> field