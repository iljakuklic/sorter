
module SortAlgo.SelectSort(selectSort) where

import SortAlgo.Algo

import Control.Monad (when)

-- | Find the index of the smallest element in given range.
findMinIdx :: Idx -> Idx -> Idx -> Sorter Idx
findMinIdx cur beg end | beg > end = return cur
findMinIdx cur beg end = do
    r <- compareAt cur beg
    findMinIdx (if r == GT then beg else cur) (beg + 1) end

-- | A simple selection sort.
selectSort :: OpenSortAlgo
selectSort _rec beg end | rangeSize beg end < 2 = return ()
selectSort rec beg end = do
    minIdx <- findMinIdx beg (succ beg) end
    when (minIdx /= beg) $ do
        swapAt beg minIdx
    rec (beg + 1) end
