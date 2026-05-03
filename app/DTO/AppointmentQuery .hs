module DTO.AppointmentQuery where

data AppointmentQuery = AppointmentQuery
  { isAdmin    :: Bool
  , customerId :: Maybe Int
  , date       :: Maybe String
  , machine    :: Maybe Int
  }