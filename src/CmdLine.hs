-- Program options and command line parsing.

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CmdLine(Algorithm(..), Size, getSize, File(..), Seed(..),
               OptionsW(..), Options, getOptions) where

import           Numeric.Natural
import           Options.Generic
import qualified Options.Applicative as O
import           System.Random

-- To represent sizes of things with a nice metavar.
newtype Size = Size Natural deriving (Generic)

getSize :: Size -> Int
getSize (Size s) = fromIntegral s

instance ParseField Size where
    readField = fmap Size readField
instance ParseRecord Size
instance ParseFields Size

-- To represent file arguments with a nice metavar.
newtype File = File FilePath deriving (Generic)

instance ParseField File where
    readField = fmap File O.str
instance ParseRecord File where
instance ParseFields File

-- Support reading random seed from the command line.
newtype Seed = Seed StdGen deriving (Generic)

instance ParseField Seed where
    readField = fmap Seed O.auto
instance ParseFields Seed
instance ParseRecord Seed where
    parseRecord = fmap getOnly parseRecord

-- List of available sorting algorithms.
data Algorithm = Bubble | BubbleSimple | Heap | Quick | Select
    deriving (Show, Generic, Bounded, Enum)

-- List of all algorithms and the corresponding string name.
algorithms :: [(String, Algorithm)]
algorithms = [ (fieldNameModifier lispCaseModifiers (show a), a)
             | a <- [minBound..maxBound] ]

instance ParseField Algorithm where
    -- Custom parsing of the algorithm option.
    readField = O.maybeReader (flip lookup algorithms)
    -- List algorithm names in the metavar.
    metavar _proxy = drop 1 (concatMap (('|':) . fst) algorithms)

instance ParseRecord Algorithm
instance ParseFields Algorithm

-- Command line options.
data OptionsW w = Options {
    -- Options for specifying the sorting algorithm
    algorithm :: w ::: Algorithm
        <?> "Sorting algorithm to use",
    smallNets :: w ::: Bool
        <?> "Use specialized sorting networks once array size drops to or under 6",
    selectSortUpto :: w ::: Maybe Size
        <?> "Use select sort for arrays smaller than specified",
    bubbleThreshold :: w ::: Maybe Size
        <?> "Switch to bubble sort when all partitions are at most this large",
    -- Options specifying the initial array
    input :: w ::: Maybe File
        <?> "Load input array form a file",
    arraySize :: w ::: Maybe Size
        <?> "Generated input array size",
    randomSeed :: w ::: Maybe Seed
        <?> "Seed the random number generator",
    nearlySorted :: w ::: Bool
        <?> "Generate an array that is already almost sorted",
    reverse :: w ::: Bool
        <?> "Reverse the array before sorting",
    -- Options for specifying output format
    output :: w ::: Maybe File
        <?> "Write the animation to a GIF file"
  } deriving (Generic)

optionModifiers :: Modifiers
optionModifiers = lispCaseModifiers { shortNameModifier = short }
  where
    short "algorithm" = Just 'a'
    short "arraySize" = Just 's'
    short "output" = Just 'o'
    short "randomSeed" = Just 'R'
    short "input" = Just 'i'
    short _ = Nothing

instance ParseRecord (OptionsW Wrapped) where
    parseRecord = parseRecordWithModifiers optionModifiers

type Options = OptionsW Unwrapped

getOptions :: IO Options
getOptions = unwrapRecord "Sorting algorithm visualizer"
