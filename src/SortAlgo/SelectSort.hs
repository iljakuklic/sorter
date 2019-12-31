
module SortAlgo.SelectSort(selectSort) where

import SortAlgo.Algo

import Control.Monad
import Data.Foldable

findMinIdx cur beg end | beg > end = return cur
findMinIdx cur beg end = do
    r <- cmpAt cur beg
    findMinIdx (if r == GT then beg else cur) (beg + 1) end

selectSort :: SortAlgo
selectSort beg end = do
    for_ [beg..end] $ \i -> do
        minIdx <- findMinIdx i (succ i) end
        when (minIdx /= i) $ do
            swapAt i minIdx
