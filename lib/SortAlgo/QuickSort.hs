
{-# LANGUAGE LambdaCase #-}

module SortAlgo.QuickSort(quickSort) where

import SortAlgo.Algo

-- | Lomuto partition scheme. Returns the final index of the pivot.
partition :: Idx -> Idx -> Idx -> Sorter Idx
partition i j piv | j >= piv = swapAt i piv >> return i
partition i j piv = do
    compareAt j piv >>= \case
        LT -> swapAt i j >> partition (i + 1) (j + 1) piv
        _ -> partition i (j + 1) piv

-- | Quick sort.
quickSort :: OpenSortAlgo
quickSort _rec beg end | rangeSize beg end < 2 =
    return ()
quickSort _rec beg end | rangeSize beg end == 2 =
    sort2 beg (beg + 1) >> return ()
quickSort rec beg end = do
    mid <- partition beg beg end
    rec beg (mid - 1)
    rec (mid + 1) end
