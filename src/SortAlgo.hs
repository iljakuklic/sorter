-- Re-export various sorting algorithms.

module SortAlgo(module SortAlgo.BubbleSort,
                module SortAlgo.SelectSort,
                module SortAlgo.QuickSort,
                module SortAlgo.SmallSort,
                SortAlgo, OpenSortAlgo,
                fix, ifSize, noSort) where

import SortAlgo.Algo
import SortAlgo.BubbleSort
import SortAlgo.SelectSort
import SortAlgo.QuickSort
import SortAlgo.SmallSort
import Data.Function

-- Dynamically select sorting algorithm depending on array size.
ifSize :: (Idx -> Bool) -> SortAlgo -> SortAlgo -> SortAlgo
ifSize cond thenSorter elseSorter beg end =
    if cond (rangeSize beg end)
        then thenSorter beg end
        else elseSorter beg end

-- Trivial sorting algorithm that does not sort anything.
noSort :: SortAlgo
noSort _beg _end = return ()
