{-# LANGUAGE DeriveGeneric #-}

module Models.WaitingQueue where

import Data.Aeson
import GHC.Generics
import Database.SQLite.Simple

data WaitingQueue = WaitingQueue
  { customerId :: Int
  , scheduledAt :: String
  , createdAt :: String
  }
  deriving (Show, Eq, Generic)

instance ToJSON WaitingQueue
instance FromJSON WaitingQueue

instance FromRow WaitingQueue where
  fromRow = WaitingQueue <$> field <*> field <*> field