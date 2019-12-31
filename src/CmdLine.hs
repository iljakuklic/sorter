-- Program options and command line parsing.

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CmdLine(Algorithm(..), Size(..),
               OptionsW(..), Options, getOptions) where

import           Numeric.Natural
import qualified Options.Applicative as O
import           Options.Generic

-- List of available sorting algorithms.
data Algorithm
    = Bubble
    | Select
    | Quick
    deriving (Read, Generic)

instance ParseField Algorithm where
    -- HACK: Use record parser to parse algorithm as a field.
    -- FIXME: This causes the -h,--help switch to be shown twice
    --        in the help message.
    parseField _help _label _short = parseRecordWithModifiers lispCaseModifiers

instance ParseRecord Algorithm
instance ParseFields Algorithm

-- To represent sizes of things with nice metavar.
newtype Size = Size { getSize :: Natural } deriving (Generic)

instance ParseField Size where
    readField = fmap Size readField

-- Command line options.
data OptionsW w = Options {
    selectSortUpto :: w ::: Maybe Size
        <?> "Use select sort for smaller sizes",
    smallOpt :: w ::: Bool
        <?> "Use specialized sort for small arrays",
    algo :: w ::: Algorithm
        <?> "Sorting algorithm to use"
  } deriving (Generic)

instance ParseRecord (OptionsW Wrapped) where
    parseRecord = parseRecordWithModifiers lispCaseModifiers

type Options = OptionsW Unwrapped

getOptions :: IO Options
getOptions = unwrapRecord "Sorting algorithm visualizer"
