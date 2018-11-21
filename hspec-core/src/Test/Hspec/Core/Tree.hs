{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Stability: unstable
module Test.Hspec.Core.Tree (
  SpecTree
, Tree (..)
, Item (..)
, specGroup
, specItem
, bimapTree
, location

, customOptions
) where

import           Prelude ()
import           Test.Hspec.Core.Compat

import           Data.CallStack
import           Data.Maybe
import           Data.Typeable

import           Test.Hspec.Core.Example
import           Test.Hspec.Core.Example.Options

-- | Internal tree data structure
data Tree c a =
    Node String [Tree c a]
  | NodeWithCleanup c [Tree c a]
  | Leaf a
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | A tree is used to represent a spec internally.  The tree is parametrize
-- over the type of cleanup actions and the type of the actual spec items.
type SpecTree a = Tree (ActionWith a) (Item a)

bimapTree :: (a -> b) -> (c -> d) -> Tree a c -> Tree b d
bimapTree g f = go
  where
    go spec = case spec of
      Node d xs -> Node d (map go xs)
      NodeWithCleanup cleanup xs -> NodeWithCleanup (g cleanup) (map go xs)
      Leaf item -> Leaf (f item)

-- |
-- @Item@ is used to represent spec items internally.  A spec item consists of:
--
-- * a textual description of a desired behavior
-- * an example for that behavior
-- * additional meta information
--
-- Everything that is an instance of the `Example` type class can be used as an
-- example, including QuickCheck properties, Hspec expectations and HUnit
-- assertions.
data Item a = Item {

  -- | Textual description of behavior
  itemRequirement :: String

  -- | Source location of the spec item
, itemLocation :: Maybe Location

  -- | A flag that indicates whether it is safe to evaluate this spec item in
  -- parallel with other spec items
, itemIsParallelizable :: Maybe Bool

  -- | A flag that indicates whether this spec item is focused
, itemIsFocused :: Bool

  -- | A parser for custome options that are accepted by this spec item
, itemOptions :: (TypeRep, Maybe (OptionsParser OptionsSet))

  -- | Example for behavior
, itemExample :: OptionsSet -> (ActionWith a -> IO ()) -> ProgressCallback -> IO Result
}

-- | The @specGroup@ function combines a list of specs into a larger spec.
specGroup :: HasCallStack => String -> [SpecTree a] -> SpecTree a
specGroup s = Node msg
  where
    msg :: HasCallStack => String
    msg
      | null s = fromMaybe "(no description given)" defaultDescription
      | otherwise = s

optionsParserFromType :: forall a. Options a => (TypeRep, Maybe (OptionsParser a))
optionsParserFromType = (typeOf (undefined :: a), optionsParser)

optionsParserFromExample :: Example a => a -> (TypeRep, Maybe (OptionsParser (Opt a)))
optionsParserFromExample _ = optionsParserFromType

customOptions:: Example a => a -> (TypeRep, Maybe (OptionsParser OptionsSet))
customOptions = fmap (fmap toCustomOptions) . optionsParserFromExample
  where

-- | The @specItem@ function creates a spec item.
specItem :: (HasCallStack, Example a) => String -> a -> SpecTree (Arg a)
specItem s e = Leaf $ Item {
    itemRequirement = requirement
  , itemLocation = location
  , itemIsParallelizable = Nothing
  , itemIsFocused = False
  , itemOptions = customOptions e
  , itemExample = safeEvaluateExample e
  }
  where
    requirement :: HasCallStack => String
    requirement
      | null s = fromMaybe "(unspecified behavior)" defaultDescription
      | otherwise = s

location :: HasCallStack => Maybe Location
location = case reverse callStack of
  (_, loc) : _ -> Just (Location (srcLocFile loc) (srcLocStartLine loc) (srcLocStartCol loc))
  _ -> Nothing

defaultDescription :: HasCallStack => Maybe String
defaultDescription = case reverse callStack of
  (_, loc) : _ -> Just (srcLocModule loc ++ "[" ++ show (srcLocStartLine loc) ++ ":" ++ show (srcLocStartCol loc) ++ "]")
  _ -> Nothing
