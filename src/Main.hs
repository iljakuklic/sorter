
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main(main) where

import           Sorter
import           SortAlgo
import qualified CmdLine as CL

import           System.Random
import           Control.Monad

-- Build a sorting procedure as specified by command line options.
makeSorter :: CL.Options -> SortAlgo
makeSorter opts beg end = do
    fix mainSorter beg end
    when (bubbleSize > 0) $ fix bubbleSort beg end
  where
    sizeOpt get = Idx (maybe 0 CL.getSize (get opts))
    selectSize = sizeOpt CL.selectSortUpto
    bubbleSize = sizeOpt CL.bubbleThreshold
    smallSpecSorter = if CL.smallNets opts then smallSort else id
    smallSelectSorter = ifSize (<= selectSize) (fix selectSort)
    smallSorter = smallSelectSorter . smallSpecSorter
    baseSorter = case CL.algorithm opts of
        CL.Select -> selectSort
        CL.Bubble -> bubbleSort
        CL.Quick -> quickSort
    mainSorter = baseSorter . smallSorter . ifSize (<= bubbleSize) noSort

main :: IO ()
main = do
    opts <- CL.getOptions
    size <- case CL.arraySize opts of
        Nothing -> randomRIO (30, 100)
        Just s -> return (CL.getSize s)
    initArray0 <-
        if CL.nearlySorted opts
            then fmap (map (max 10) . drop 1 . scanl (+) 30)
                     (replicateM size (randomRIO (-5, 10)))
            else replicateM size (randomRIO (10, 300))
    let initArray = (if CL.reverse opts then reverse else id) initArray0
    let sorter = makeSorter opts
    case CL.output opts of
        Nothing -> animateInWindow (900, 600) sorter initArray
        Just (CL.File fname) -> animateGif (400, 250) fname sorter initArray
