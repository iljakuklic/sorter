-- | Re-export various sorting algorithms.

module SortAlgo(module SortAlgo.BubbleSort,
                module SortAlgo.SelectSort,
                module SortAlgo.QuickSort,
                module SortAlgo.HeapSort,
                module SortAlgo.SmallSort,
                SortAlgo, OpenSortAlgo,
                fix, ifSize, noSort, sortFocuser) where

import SortAlgo.Algo
import SortAlgo.BubbleSort
import SortAlgo.SelectSort
import SortAlgo.QuickSort
import SortAlgo.HeapSort
import SortAlgo.SmallSort
import Data.Function (fix)

-- | Dynamically select sorting algorithm depending on array size.
ifSize :: (Idx -> Bool) -> SortAlgo -> SortAlgo -> SortAlgo
ifSize cond thenSorter elseSorter beg end =
    if cond (rangeSize beg end)
        then thenSorter beg end
        else elseSorter beg end

-- | Trivial sorting algorithm that does not sort anything.
noSort :: SortAlgo
noSort _beg _end = return ()

-- | Focus the range being sorted before sorting.
sortFocuser :: SortAlgo -> SortAlgo
sortFocuser theSort beg end = focusRange beg end >> theSort beg end
