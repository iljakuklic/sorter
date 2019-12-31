
module SortAlgo.BubbleSort(bubbleSortSimple, bubbleSort) where

import SortAlgo.Algo

import Data.Foldable
import Data.Monoid

-- Naive bubble sort.
bubbleSortSimple :: SortAlgo
bubbleSortSimple beg end = do
    for_ [end,end-1..2] $ \stop ->
        for_ [beg..(stop - 1)] $ \i ->
            sort2 i (i + 1)

-- Bubble sort with a bit more sophisticated way of determining
-- what part of the array shall be processed in each pass.
bubbleSort :: OpenSortAlgo
bubbleSort rec beg end = do
    Last ii <- flip foldMap [beg..(end - 1)] $ \i -> do
        sw <- sort2 i (i + 1)
        return (Last (if sw then Just i else Nothing))
    case ii of
        Just sw -> rec beg sw
        Nothing -> return ()
