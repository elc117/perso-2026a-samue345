{-# LANGUAGE DeriveGeneric #-}

module DTO.CheckInRequest where

import Data.Aeson
import GHC.Generics

data CheckInRequest = CheckInRequest
  { 
    password :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON CheckInRequest