-- | Various definitions to support implementing sorting algorithms.

{-# LANGUAGE LambdaCase #-}

module SortAlgo.Algo(module Sorter.Spec, OpenSortAlgo, SortAlgo,
                     sort2, rangeSize) where

import Sorter.Spec

-- | Sort algorithm takes array bounds and produces a sorter.
-- Unline in C (and like in Data.Arry), the bounds are inclusive.
type SortAlgo = Idx -> Idx -> Sorter ()

-- | Sorting algorithm with open recursion for greater flexibility.
type OpenSortAlgo = SortAlgo -> SortAlgo

-- | Calculate range size given by a pair of indices indicating
-- range beginning and end.
rangeSize :: Idx -> Idx -> Idx
rangeSize beg end | end < beg = 0
rangeSize beg end = end - beg + 1

-- | Sort elements on two indices `i`, `j`.
--
-- Returns whether the elements have been swapped. Smaller element will
-- always be placed on index `i`, even if `j` preceeds `i`.
sort2 :: Idx -> Idx -> Sorter Bool
sort2 i j = do
    compareAt i j >>= \case
        GT -> swapAt i j >> return True
        _  -> return False
