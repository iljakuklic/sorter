-- Re-export various sorting algorithms.

module SortAlgo(module SortAlgo.BubbleSort,
                module SortAlgo.SelectSort,
                module SortAlgo.QuickSort,
                SortAlgo, OpenSortAlgo,
                fix, ifSize, noSort) where

import SortAlgo.Algo
import SortAlgo.BubbleSort
import SortAlgo.SelectSort
import SortAlgo.QuickSort
import Data.Function

-- Dynamically select sorting algorithm depending on array size.
ifSize :: (Idx -> Bool) -> SortAlgo -> SortAlgo -> SortAlgo
ifSize pred thenSorter elseSorter beg end =
    if pred (end - beg + 1)
        then thenSorter beg end
        else elseSorter beg end

-- Trivial sorting algorithm that does not sort anything.
noSort :: SortAlgo
noSort _ _ = return ()
