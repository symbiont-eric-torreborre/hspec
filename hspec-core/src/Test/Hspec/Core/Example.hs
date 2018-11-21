{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Test.Hspec.Core.Example (
  Example (..)
, Params (..)
, defaultParams
, ActionWith
, Progress
, ProgressCallback
, Result(..)
, ResultStatus (..)
, Location (..)
, FailureReason (..)
, safeEvaluateExample

, QuickCheckOptions(..)
, toQuickCheckArgs
, qqMaxSuccess
, qqMaxSize
, qqMaxDiscardRatio
, qqMaxShrinks
, qqSeed
) where

import qualified Test.HUnit.Lang as HUnit

import           Data.CallStack

import           Control.Exception
import           Control.DeepSeq
import           Data.Typeable (Typeable)
import           Data.Maybe
import qualified Test.QuickCheck as QC
import           Test.Hspec.Expectations (Expectation)

import qualified Test.QuickCheck.State as QC (numSuccessTests, maxSuccessTests)
import qualified Test.QuickCheck.Property as QCP

import           Test.Hspec.Core.QuickCheckUtil
import           Test.Hspec.Core.Util
import           Test.Hspec.Core.Compat
import           Test.Hspec.Core.Example.Location
import           Test.Hspec.Core.Example.Options

-- | A type class for examples
class Options (Opt e) => Example e where
  type Arg e
  type Arg e = ()

  type Opt e
  type Opt e = ()

  evaluateExample :: e -> Opt e -> (ActionWith (Arg e) -> IO ()) -> ProgressCallback -> IO Result

data Params = Params {
  paramsQuickCheckArgs  :: QC.Args
, paramsSmallCheckDepth :: Int
} deriving (Show)

defaultParams :: Params
defaultParams = Params {
  paramsQuickCheckArgs = QC.stdArgs
, paramsSmallCheckDepth = 5
}

type Progress = (Int, Int)
type ProgressCallback = Progress -> IO ()

-- | An `IO` action that expects an argument of type @a@
type ActionWith a = a -> IO ()

-- | The result of running an example
data Result = Result {
  resultInfo :: String
, resultStatus :: ResultStatus
} deriving (Show, Typeable)

data ResultStatus =
    Success
  | Pending (Maybe Location) (Maybe String)
  | Failure (Maybe Location) FailureReason
  deriving (Show, Typeable)

data FailureReason =
    NoReason
  | Reason String
  | ExpectedButGot (Maybe String) String String
  | Error (Maybe String) SomeException
  deriving (Show, Typeable)

instance NFData FailureReason where
  rnf reason = case reason of
    NoReason -> ()
    Reason r -> r `deepseq` ()
    ExpectedButGot p e a  -> p `deepseq` e `deepseq` a `deepseq` ()
    Error m e -> m `deepseq` e `seq` ()

instance Exception ResultStatus

safeEvaluateExample :: Example e => e -> OptionsSet -> (ActionWith (Arg e) -> IO ()) -> ProgressCallback -> IO Result
safeEvaluateExample example opts around progress = do
  r <- safeTry $ forceResult <$> evaluateExample example (getOptions opts) around progress
  return $ case r of
    Left e | Just result <- fromException e -> Result "" result
    Left e | Just hunit <- fromException e -> Result "" $ hunitFailureToResult Nothing hunit
    Left e -> Result "" $ Failure Nothing $ Error Nothing e
    Right result -> result
  where
    forceResult :: Result -> Result
    forceResult r@(Result info status) = info `deepseq` (forceResultStatus status) `seq` r

    forceResultStatus :: ResultStatus -> ResultStatus
    forceResultStatus r = case r of
      Success -> r
      Pending _ m -> m `deepseq` r
      Failure _ m -> m `deepseq` r

instance Example Result where
  type Arg Result = ()
  evaluateExample e = evaluateExample (\() -> e)

instance Example (a -> Result) where
  type Arg (a -> Result) = a
  evaluateExample example _ action _callback = do
    ref <- newIORef (Result "" Success)
    action (evaluate . example >=> writeIORef ref)
    readIORef ref

instance Example Bool where
  type Arg Bool = ()
  evaluateExample e = evaluateExample (\() -> e)

instance Example (a -> Bool) where
  type Arg (a -> Bool) = a
  evaluateExample p _ action _callback = do
    ref <- newIORef (Result "" Success)
    action (evaluate . example >=> writeIORef ref)
    readIORef ref
    where
      example a
        | p a = Result "" Success
        | otherwise = Result "" $ Failure Nothing NoReason

instance Example Expectation where
  type Arg Expectation = ()
  evaluateExample e = evaluateExample (\() -> e)

hunitFailureToResult :: Maybe String -> HUnit.HUnitFailure -> ResultStatus
hunitFailureToResult pre e = case e of
  HUnit.HUnitFailure mLoc err ->
      case err of
        HUnit.Reason reason -> Failure location (Reason $ addPre reason)
        HUnit.ExpectedButGot preface expected actual -> Failure location (ExpectedButGot (addPreMaybe preface) expected actual)
          where
            addPreMaybe :: Maybe String -> Maybe String
            addPreMaybe xs = case (pre, xs) of
              (Just x, Just y) -> Just (x ++ "\n" ++ y)
              _ -> pre <|> xs
    where
      location = case mLoc of
        Nothing -> Nothing
        Just loc -> Just $ Location (srcLocFile loc) (srcLocStartLine loc) (srcLocStartCol loc)
  where
    addPre :: String -> String
    addPre xs = case pre of
      Just x -> x ++ "\n" ++ xs
      Nothing -> xs

instance Example (a -> Expectation) where
  type Arg (a -> Expectation) = a
  evaluateExample e _ action _ = action e >> return (Result "" Success)

instance Example QC.Property where
  type Arg QC.Property = ()
  type Opt QC.Property = QuickCheckOptions
  evaluateExample e = evaluateExample (\() -> e)

instance Example (a -> QC.Property) where
  type Arg (a -> QC.Property) = a
  type Opt (a -> QC.Property) = QuickCheckOptions
  evaluateExample p args action progressCallback = do
    r <- QC.quickCheckWithResult (toQuickCheckArgs args) {QC.chatty = False} (QCP.callback qcProgressCallback $ aroundProperty action p)
    return $ fromQuickCheckResult r
    where
      qcProgressCallback = QCP.PostTest QCP.NotCounterexample $
        \st _ -> progressCallback (QC.numSuccessTests st, QC.maxSuccessTests st)

fromQuickCheckResult :: QC.Result -> Result
fromQuickCheckResult r = case parseQuickCheckResult r of
  QuickCheckResult _ info (QuickCheckOtherFailure err) -> Result info $ Failure Nothing (Reason err)
  QuickCheckResult _ info QuickCheckSuccess -> Result info Success
  QuickCheckResult n info (QuickCheckFailure QCFailure{..}) -> case quickCheckFailureException of
    Just e | Just result <- fromException e -> Result info result
    Just e | Just hunit <- fromException e -> Result info $ hunitFailureToResult (Just hunitAssertion) hunit
    Just e -> failure (uncaughtException e)
    Nothing -> failure falsifiable
    where
      failure = Result info . Failure Nothing . Reason

      numbers = formatNumbers n quickCheckFailureNumShrinks

      hunitAssertion :: String
      hunitAssertion = intercalate "\n" [
          "Falsifiable " ++ numbers ++ ":"
        , indent (unlines quickCheckFailureCounterexample)
        ]

      uncaughtException e = intercalate "\n" [
          "uncaught exception: " ++ formatException e
        , numbers
        , indent (unlines quickCheckFailureCounterexample)
        ]

      falsifiable = intercalate "\n" [
          quickCheckFailureReason ++ " " ++ numbers ++ ":"
        , indent (unlines quickCheckFailureCounterexample)
        ]

indent :: String -> String
indent = intercalate "\n" . map ("  " ++) . lines

data QuickCheckOptions = QuickCheckOptions {
  qMaxSuccess :: Maybe Int
, qMaxSize :: Maybe Int
, qMaxDiscardRatio :: Maybe Int
, qMaxShrinks :: Maybe Int
, qSeed :: Maybe Int
} deriving (Eq, Show)

instance Options QuickCheckOptions where
  defaultOptions = QuickCheckOptions {
        qMaxSuccess = Nothing
      , qMaxSize = Nothing
      , qMaxDiscardRatio = Nothing
      , qMaxShrinks = Nothing
      , qSeed = Nothing
      }
  optionsParser = options "OPTIONS FOR QUICKCHECK" [
      flag (Just 'a') "qc-max-success" "N" (withRead setMaxSuccess)
        "maximum number of successful tests before a QuickCheck property succeeds"
    , flag Nothing "qc-max-size" "N" (withRead setMaxSize)
        "size to use for the biggest test cases"
    , flag Nothing "qc-max-discard" "N" (withRead setMaxDiscardRatio)
        "maximum number of discarded tests per successful test before giving up"
    , flag Nothing "seed" "N" (withRead setSeed)
        "used seed for QuickCheck properties"
    ]


toQuickCheckArgs :: QuickCheckOptions -> QC.Args
toQuickCheckArgs opts = QC.stdArgs {
  QC.maxSuccess = qqMaxSuccess opts
, QC.maxSize = qqMaxSize opts
, QC.maxDiscardRatio = qqMaxDiscardRatio opts
, QC.maxShrinks = qqMaxShrinks opts
, QC.replay = Just (mkGen (qqSeed opts), 0)
}

qqSeed :: QuickCheckOptions -> Int
qqSeed = fromMaybe 0 . qSeed

setMaxSuccess :: Int -> QuickCheckOptions -> QuickCheckOptions
setMaxSuccess n args = args {qMaxSuccess = Just n}

setMaxSize :: Int -> QuickCheckOptions -> QuickCheckOptions
setMaxSize n args = args {qMaxSize = Just n}

setMaxDiscardRatio :: Int -> QuickCheckOptions -> QuickCheckOptions
setMaxDiscardRatio n args = args {qMaxDiscardRatio = Just n}

setSeed :: Int -> QuickCheckOptions -> QuickCheckOptions
setSeed n args = args {qSeed = Just n}

qqMaxSuccess :: QuickCheckOptions -> Int
qqMaxSuccess = fromMaybe (QC.maxSuccess QC.stdArgs) . qMaxSuccess

qqMaxSize :: QuickCheckOptions -> Int
qqMaxSize = fromMaybe (QC.maxSize QC.stdArgs) . qMaxSize

qqMaxDiscardRatio :: QuickCheckOptions -> Int
qqMaxDiscardRatio = fromMaybe (QC.maxDiscardRatio QC.stdArgs) . qMaxDiscardRatio

qqMaxShrinks :: QuickCheckOptions -> Int
qqMaxShrinks = fromMaybe (QC.maxShrinks QC.stdArgs) . qMaxShrinks


