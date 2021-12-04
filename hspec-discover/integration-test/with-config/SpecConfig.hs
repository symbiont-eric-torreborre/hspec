module SpecConfig where

import Test.Hspec.Runner
import Test.Hspec.Core.Extension
import Test.Hspec.Core.Formatters.V2

config :: SpecConfig ()
config = modifyConfig $ \ c -> c {
  configFormat = Just $ formatterToFormat progress
}
