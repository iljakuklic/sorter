-- | Implements sorter in terms of mutable arrays and keeps a log of
-- actions performed for later visualization.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Sorter.Runner(runSort) where

import Sorter.Spec

import Data.Array.Unboxed
import Data.Array.ST
import Control.Applicative
import Control.Monad.ST
import Control.Monad.Trans
import Control.Monad.RWS

-- Unboxed mutable array of Ints indexed by Idx.
type ArrayTy s = STUArray s Idx Int

-- Reader keeps the array reference, writer logs the actions.
type SorterMonad s = RWST (ArrayTy s) [AnAction] () (ST s)

-- Perform an action on an array.
handleAction :: Action a -> ArrayTy s -> ST s a
handleAction (PeekAt i) arr = readArray arr i
handleAction (CmpAt i j) arr =
    liftA2 compare (readArray arr i) (readArray arr j)
handleAction (SwapAt i j) arr = do
    xi <- readArray arr i
    xj <- readArray arr j
    writeArray arr i xj
    writeArray arr j xi

-- Log and perform given action.
handleAndLogAction :: Action a -> SorterMonad s a
handleAndLogAction a = do
    tell [AnAction a]
    ask >>= lift . handleAction a

-- | Run the sorting algorithm on given immutable array.
runSort :: (IArray iarr Int)
        => (Idx -> Idx -> Sorter a)
        -- ^ Sorting algorithm to run
        -> iarr Idx Int
        -- ^ Input array
        -> (a, iarr Idx Int, [AnAction])
        -- ^ (sorter result, final sorted array, action log)
runSort sorter inAry = runST $ do
    -- First convert to a mutable array.
    ary <- thaw inAry
    -- Get array size to pass to the sorting algorithm.
    (lo, hi) <- getBounds ary
    -- Run the sorting algorithm in RWS monad.
    let sorting = runSorter handleAndLogAction (sorter lo hi)
    (res, actions) <- evalRWST sorting ary ()
    -- Get the resulting array
    outAry <- freeze ary
    -- Return sorter result, the final array, and the action log.
    return (res, outAry, actions)
