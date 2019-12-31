
{-# LANGUAGE LambdaCase #-}

module SortAlgo.QuickSort(quickSort) where

import SortAlgo.Algo

import Control.Monad
import Data.Foldable

-- Lomuto partition scheme. Returns the final index of the pivot.
partition :: Idx -> Idx -> Idx -> Sorter Idx
partition i j piv | j >= piv = swapAt i piv >> return i
partition i j piv = do
    cmpAt j piv >>= \case
        LT -> swapAt i j >> partition (i + 1) (j + 1) piv
        _ -> partition i (j + 1) piv

-- Quick sort.
quickSort :: SortAlgo
quickSort beg end | beg >= end = return ()
quickSort beg end | beg + 1 == end = sort2 beg (beg + 1) >> return ()
quickSort beg end = do
    mid <- partition beg beg end
    quickSort beg (mid - 1)
    quickSort (mid + 1) end
