module Main where

import Test.Hspec

import qualified Repository.AppointmentRepositorySpec as AppointmentRepositorySpec
import qualified Service.AppointmentServiceSpec as AppointmentServiceSpec
import qualified Handler.AppointmentHandlerSpec as AppointmentHandlerSpec

main :: IO ()
main = hspec $ do
  AppointmentRepositorySpec.spec
  AppointmentServiceSpec.spec
  AppointmentHandlerSpec.spec