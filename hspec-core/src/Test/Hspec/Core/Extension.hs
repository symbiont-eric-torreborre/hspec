{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.Hspec.Core.Extension (
  SpecConfig
, runIO
, modifyConfig
, runSpecConfig
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Control.Monad.Trans.Writer
import           Control.Monad.IO.Class

import           Test.Hspec.Core.Runner (Config)

newtype SpecConfig a = SpecConfig { unSpecConfig :: WriterT (Endo Config) IO a }
  deriving (Functor, Applicative, Monad)

{-# WARNING runSpecConfig "This function is used by @hspec-discover@.  It is not part of the public API and may change at any time." #-}
runSpecConfig :: SpecConfig () -> IO (Config -> Config)
runSpecConfig = fmap appEndo . execWriterT . unSpecConfig

runIO :: IO a -> SpecConfig a
runIO = SpecConfig . liftIO

modifyConfig :: (Config -> Config) -> SpecConfig ()
modifyConfig = SpecConfig . tell . Endo
