{-# LANGUAGE DeriveGeneric #-}

module Models.Appointment where

import Data.Aeson
import GHC.Generics
import Database.SQLite.Simple


data Appointment = Appointment
  { appointmentId :: Int
  , customerId :: Int
  , scheduledAt :: String
  , machine :: Int
  , password :: String
  , status :: String
  }
  deriving (Show, Eq, Generic)

instance ToJSON Appointment
instance FromJSON Appointment

instance FromRow Appointment where
  fromRow =
    Appointment <$> field <*> field <*> field <*> field <*> field <*> field