
{-# LANGUAGE LambdaCase #-}

module SortAlgo.Algo(module Sorter.Spec, SortAlgo, sort2) where

import Sorter.Spec

type SortAlgo = Idx -> Idx -> Sorter ()

sort2 i j = do
    cmpAt i j >>= \case
        GT -> swapAt i j >> return True
        _  -> return False
