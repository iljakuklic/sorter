
{-# LANGUAGE LambdaCase #-}

module SortAlgo.QuickSort(quickSort) where

import SortAlgo.Algo

import Control.Monad
import Data.Foldable


partition i j piv | j >= piv = swapAt i piv >> return i
partition i j piv = do
    cmpAt j piv >>= \case
        LT -> swapAt i j >> partition (i + 1) (j + 1) piv
        _ -> partition i (j + 1) piv

quickSort beg end | beg >= end = return ()
quickSort beg end | beg + 1 == end = sort2 beg (beg + 1) >> return ()
-- quickSort beg end | beg + 2 == end = sort3 beg (beg + 1) (beg + 2)
quickSort beg end = do
    mid <- partition beg beg end
    quickSort beg (mid - 1)
    quickSort (mid + 1) end
