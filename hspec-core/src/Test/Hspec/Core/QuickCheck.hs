-- | Stability: provisional
module Test.Hspec.Core.QuickCheck (
  modifyArgs
, modifyMaxSuccess
, modifyMaxDiscardRatio
, modifyMaxSize
, modifyMaxShrinks
) where

import           Test.QuickCheck
import           Test.Hspec.Core.Example.Options
import           Test.Hspec.Core.Spec

-- | Use a modified `maxSuccess` for given spec.
modifyMaxSuccess :: (Int -> Int) -> SpecWith a -> SpecWith a
modifyMaxSuccess = modifyArgs . modify
  where
    modify :: (Int -> Int) -> QuickCheckOptions -> QuickCheckOptions
    modify f args = args {qMaxSuccess = Just $ f (qqMaxSuccess args)}

-- | Use a modified `maxDiscardRatio` for given spec.
modifyMaxDiscardRatio :: (Int -> Int) -> SpecWith a -> SpecWith a
modifyMaxDiscardRatio = modifyArgs . modify
  where
    modify :: (Int -> Int) -> QuickCheckOptions -> QuickCheckOptions
    modify f args = args {qMaxDiscardRatio = Just $ f (qqMaxDiscardRatio args)}

-- | Use a modified `maxSize` for given spec.
modifyMaxSize :: (Int -> Int) -> SpecWith a -> SpecWith a
modifyMaxSize = modifyArgs . modify
  where
    modify :: (Int -> Int) -> QuickCheckOptions -> QuickCheckOptions
    modify f args = args {qMaxSize = Just $ f (qqMaxSize args)}

-- | Use a modified `maxShrinks` for given spec.
modifyMaxShrinks :: (Int -> Int) -> SpecWith a -> SpecWith a
modifyMaxShrinks = modifyArgs . modify
  where
    modify :: (Int -> Int) -> QuickCheckOptions -> QuickCheckOptions
    modify f args = args {qMaxShrinks = Just $ f (qqMaxShrinks args)}

-- | Use modified `QuickCheckOptions` for given spec.
modifyArgs :: (QuickCheckOptions -> QuickCheckOptions) -> SpecWith a -> SpecWith a
modifyArgs = modifyParams
