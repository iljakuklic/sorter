
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main(main) where

import           Sorter
import           SortAlgo
import qualified CmdLine as CL

import           System.Random
import           Control.Monad

makeSorter :: CL.Options -> SortAlgo
makeSorter opts = fix (baseSorter . smallSelectSorter . smallSorter)
  where
    smallSorter = if CL.smallOpt opts then smallSort else id
    selectSize = fromIntegral (maybe 0 CL.getSize (CL.selectSortUpto opts))
    smallSelectSorter = ifSize (<= selectSize) (fix selectSort)
    baseSorter = case CL.algo opts of
      CL.Select -> selectSort
      CL.Bubble -> bubbleSort
      CL.Quick -> quickSort

main :: IO ()
main = do
    opts <- CL.getOptions
    randomRIO (30, 50)
         >>= flip replicateM (randomRIO (10, 300))
         >>= animateInWindow (900, 600) (makeSorter opts)
