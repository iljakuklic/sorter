-- Program options and command line parsing.

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CmdLine(Algorithm(..), Size, getSize, File(..),
               OptionsW(..), Options, getOptions) where

import           Numeric.Natural
import           Data.Proxy
import           Options.Generic
import qualified Options.Applicative as O
import qualified Options.Applicative.Types as O
import qualified Data.Text as T

-- To represent sizes of things with nice metavar.
newtype Size = Size Natural deriving (Read, Generic)

getSize :: Size -> Int
getSize (Size s) = fromIntegral s

instance ParseField Size where
    readField = fmap Size readField
instance ParseRecord Size
instance ParseFields Size

-- To represent file arguments with a nice metavar.
newtype File = File FilePath deriving (Read, Generic)

instance ParseField File where
    readField = fmap File readField
instance ParseRecord File
instance ParseFields File

-- List of available sorting algorithms.
data Algorithm = Bubble | Select | Quick
    deriving (Show, Generic, Bounded, Enum)

-- List of all algorithms and the corresponding string name.
algorithms :: [(String, Algorithm)]
algorithms = [ (fieldNameModifier lispCaseModifiers (show a), a)
             | a <- [minBound..maxBound] ]

-- Join a list of strings with commas.
commaJoin :: [String] -> String
commaJoin = drop 2 . concatMap (", " <>)

instance ParseField Algorithm where
    -- Custom command line processing for the algorithm option.
    parseField help label short = O.option readField fs
      where
        fs = foldMap (O.help . (<> algoStr) . T.unpack) help
          <> foldMap (O.long . T.unpack) label
          <> foldMap O.short short
          <> O.metavar (metavar (Proxy :: Proxy Algorithm))
        algoNames = commaJoin (map fst algorithms)
        algoStr = " (" <> algoNames <> ")"

    -- Custom parsing of the algorithm option.
    readField = do
        str <- O.readerAsk
        case lookup str algorithms of
            Just alg -> return alg
            Nothing -> O.readerError ("Unknown algorithm: " <> str)

instance ParseRecord Algorithm
instance ParseFields Algorithm

-- Command line options.
data OptionsW w = Options {
    -- Options for specifying the sorting algorithm
    algorithm :: w ::: Algorithm
        <?> "Sorting algorithm to use",
    smallOpt :: w ::: Bool
        <?> "Use specialized sort once array size drops to 6",
    selectSortUpto :: w ::: Maybe Size
        <?> "Use select sort for arrays smaller than specified",
    bubbleThreshold :: w ::: Maybe Size
        <?> "Switch to bubble sort when all partitions are at most this large",
    -- Options specifying the initial array
    arraySize :: w ::: Maybe Size
        <?> "Generated input array size",
    nearlySorted :: w ::: Bool
        <?> "Generate an array thet is already almost sorted",
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
    short "reverse" = Just 'R'
    short _ = Nothing

instance ParseRecord (OptionsW Wrapped) where
    parseRecord = parseRecordWithModifiers optionModifiers

type Options = OptionsW Unwrapped

getOptions :: IO Options
getOptions = unwrapRecord "Sorting algorithm visualizer"
