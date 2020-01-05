
{-# LANGUAGE LambdaCase #-}

module SortAlgo.HeapSort(heapify, heapSort) where

import SortAlgo.Algo
import SortAlgo.SmallSort

import Control.Applicative
import Control.Monad
import Data.Function (fix)

-- Calculate parent, left child, right child index in the heap.
-- The begining index of the heap has to be passed too.
leftIdx, rightIdx :: Idx -> Idx -> Idx
leftIdx beg i = i * 2 - beg + 1
rightIdx beg i = leftIdx beg i + 1

siftDown :: Idx -> Idx -> Idx -> Sorter ()
siftDown beg root end = do
    let lid = leftIdx beg root
    let rid = rightIdx beg root
    let continue next = swapAt root next >> siftDown beg next end
    let cmp i = if i <= end then compareAt root i else return GT
    liftA2 (,) (cmp lid) (cmp rid) >>= \case
        (LT, LT) -> do
            c <- liftA2 (>=) (peekAt lid) (peekAt rid)
            continue (if c then lid else rid)
        (LT, _ ) -> continue lid
        (_ , LT) -> continue rid
        (_ , _ ) -> return ()

-- | Turn an array into a max-heap.
heapify :: SortAlgo
heapify beg end = do
    let from = (beg + end) `div` 2
    forM_ [from,from-1..beg] $ \i -> do
        focusRange i end
        siftDown beg i end

-- | Heap sort
heapSort :: SortAlgo
heapSort beg end | rangeSize beg end < 4 = fix smallSort beg end
heapSort beg end = do
    heapify beg end
    swapAt beg end
    forM_ [end-1,end-2..(beg+3)] $ \i -> do
        focusRange beg i
        siftDown beg beg i
        swapAt beg i
    -- Clean up the last three elements.
    -- We already know that the element at position 1 is greater than
    -- the element at position 0 because of how the heap was structured.
    focusRange beg (beg+2)
    void $ sort2 (beg+1) (beg+2)
    void $ sort2 (beg+0) (beg+1)
