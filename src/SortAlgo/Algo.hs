-- Various definititons to support implementing sorting algorithms.

{-# LANGUAGE LambdaCase #-}

module SortAlgo.Algo(module Sorter.Spec, SortAlgo, sort2) where

import Sorter.Spec

-- Sort algorithm takes array bounds and produces a sorter.
type SortAlgo = Idx -> Idx -> Sorter ()

-- Sort elements on two indices i, j.
-- Returns whether the elements have been swapped.
sort2 i j = do
    cmpAt i j >>= \case
        GT -> swapAt i j >> return True
        _  -> return False
