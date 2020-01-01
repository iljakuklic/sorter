-- Program options and command line parsing.

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CmdLine(Algorithm(..), Size, getSize,
               OptionsW(..), Options, getOptions) where

import           Numeric.Natural
import           Options.Generic

-- To represent sizes of things with nice metavar.
newtype Size = Size Natural deriving (Read, Generic)

getSize :: Size -> Int
getSize (Size s) = fromIntegral s

instance ParseField Size where
    readField = fmap Size readField
instance ParseRecord Size
instance ParseFields Size

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

-- Command line options.
data OptionsW w = Options {
    -- Options specifying the initial array
    arraySize :: w ::: Maybe Size
        <?> "Generated input array size",
    nearlySorted :: w ::: Bool
        <?> "Generate an array thet is already almost sorted",
    -- Options for specifying output format
    output :: w ::: Maybe FilePath
        <?> "Write the animation to a GIF file",
    -- Options for specifying the sorting algorithm
    smallOpt :: w ::: Bool
        <?> "Use specialized sort for small arrays",
    selectSortUpto :: w ::: Maybe Size
        <?> "Use select sort for smaller sizes",
    bubbleThreshold :: w ::: Maybe Size
        <?> "Switch to bubble sort when all partitions are at most this large",
    algorithm :: w ::: Algorithm
        <?> "Sorting algorithm to use"
  } deriving (Generic)

optionModifiers :: Modifiers
optionModifiers = lispCaseModifiers { shortNameModifier = short }
  where
    short "algorithm" = Just 'a'
    short "arraySize" = Just 's'
    short "output" = Just 'o'
    short _ = Nothing

instance ParseRecord (OptionsW Wrapped) where
    parseRecord = parseRecordWithModifiers optionModifiers

type Options = OptionsW Unwrapped

getOptions :: IO Options
getOptions = unwrapRecord "Sorting algorithm visualizer"
