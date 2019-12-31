
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main(main) where

import           Sorter
import           SortAlgo
import qualified CmdLine as CL

import           System.Random
import           Control.Monad

makeSorter :: CL.Options -> SortAlgo
makeSorter opts beg end = do
    fix mainSorter beg end
    when (bubbleSize > 0) $ fix bubbleSort beg end
  where
    sizeOpt get = Idx (maybe 0 CL.getSize (get opts))
    selectSize = sizeOpt CL.selectSortUpto
    bubbleSize = sizeOpt CL.bubbleThreshold
    smallSpecSorter = if CL.smallOpt opts then smallSort else id
    smallSelectSorter = ifSize (<= selectSize) (fix selectSort)
    smallSorter = smallSelectSorter . smallSpecSorter
    baseSorter = case CL.algo opts of
      CL.Select -> selectSort
      CL.Bubble -> bubbleSort
      CL.Quick -> quickSort
    mainSorter = baseSorter . smallSorter . ifSize (<= bubbleSize) noSort

main :: IO ()
main = do
    opts <- CL.getOptions
    size <- case CL.size opts of
        Nothing -> randomRIO (30, 100)
        Just s -> return (CL.getSize s)
    replicateM size (randomRIO (10, 300))
        >>= animateInWindow (900, 600) (makeSorter opts)
