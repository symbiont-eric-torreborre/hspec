{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
module Test.Hspec.Discover {-# WARNING
  "This module is used by @hspec-discover@.  It is not part of the public API and may change at any time."
  #-} (
  Spec
, hspec
, hspecWithExtensions
, IsFormatter (..)
, hspecWithFormatter
, postProcessSpec
, describe
, module Prelude
) where

import           Test.Hspec.Core.Spec
import           Test.Hspec.Core.Runner
import           Test.Hspec.Core.Extension
import           Test.Hspec.Formatters

hspecWithExtensions :: SpecConfig () -> Spec -> IO ()
hspecWithExtensions extensions spec = do
  f <- runSpecConfig extensions
  hspecWith (f defaultConfig) spec

class IsFormatter a where
  toFormatter :: a -> IO Formatter

instance IsFormatter (IO Formatter) where
  toFormatter = id

instance IsFormatter Formatter where
  toFormatter = return

hspecWithFormatter :: IsFormatter a => a -> Spec -> IO ()
hspecWithFormatter formatter spec = do
  f <- toFormatter formatter
  hspecWith defaultConfig {configFormatter = Just f} spec

postProcessSpec :: FilePath -> Spec -> Spec
postProcessSpec _ = id
