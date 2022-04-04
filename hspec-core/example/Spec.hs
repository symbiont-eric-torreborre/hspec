module Main (main, spec) where

import Prelude
import Test.Hspec.Core.Spec
import Test.Hspec.Core.Runner
import Test.Hspec.Expectations
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "reverse" $ do
    it (take 182 $ cycle ['0' .. '9']) $ do
      reverse [1 :: Int, 2, 3] `shouldBe` [3, 2, 1]

    it "reverses a list" $ do
      -- print "foobar baz"
      reverse [1 :: Int, 2, 3] `shouldBe` [3, 2, 1]

    -- it "gives the original list, if applied twice gives the original list, if applied twice gives the original list, if applied twice " $ property $
    -- it "gives the original list, if applied twice gives the original list, if applied twice gives the original list, if applied twice gives the original list, if applied twice gives the original list, if applied twice " $ property $
    it "gives the original list, if applied twice gives the original list, if applied twice gives the original list, if applied twice gives the original list, if applied twice gives the" $ property $
      \xs -> (reverse . reverse) xs == (xs :: [Int])
