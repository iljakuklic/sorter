
{-# LANGUAGE LambdaCase #-}

module SortAlgo.HeapSort(heapify, heapSort) where

import SortAlgo.Algo
import SortAlgo.SmallSort

import Control.Applicative
import Control.Monad
import Data.Function (fix)

-- Calculate parent, left child, right child index in the heap.
leftIdx, rightIdx :: Idx -> Idx
leftIdx i = i * 2 + 1
rightIdx i = i * 2 + 2

siftDown :: Idx -> Idx -> Sorter ()
siftDown root end = do
    let lid = leftIdx root
    let rid = rightIdx root
    let continue next = swapAt root next >> siftDown next end
    let cmp i = if i <= end then compareAt root i else return GT
    liftA2 (,) (cmp lid) (cmp rid) >>= \case
        (LT, LT) -> do
            c <- liftA2 (>=) (peekAt lid) (peekAt rid)
            continue (if c then lid else rid)
        (LT, _ ) -> continue lid
        (_ , LT) -> continue rid
        (_ , _ ) -> return ()

-- | Turn an array into a max-heap.
--
-- This assumes the array starts at index 0.
heapify :: SortAlgo
heapify 0 end = do
    let from = end `div` 2
    forM_ [from,from-1..0] $ \i -> do
        focusRange i end
        siftDown i end
heapify _ _ = error "heapify: array must start at 0"

-- | Heap sort
--
-- Array must start at index 0.
heapSort :: SortAlgo
heapSort 0 end | rangeSize 0 end < 4 = fix smallSort 0 end
heapSort 0 end = do
    heapify 0 end
    swapAt 0 end
    forM_ [end-1,end-2..3] $ \i -> do
        focusRange 0 i
        siftDown 0 i
        swapAt 0 i
    -- Clean up the last three elements.
    -- We already know that the element at position 1 is greater than
    -- the element at position 0 because of how the heap was structured.
    focusRange 0 2
    void $ sort2 1 2
    void $ sort2 0 1
heapSort _ _ = error "heapSort: array must start at 0"
