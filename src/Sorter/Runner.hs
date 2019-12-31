
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Sorter.Runner(runSort) where

import Sorter.Spec

import Data.Array.Unboxed
import Data.Array.ST
import Control.Applicative
import Control.Monad.ST
import Control.Monad
import Control.Monad.Trans
import Control.Monad.RWS

type ArrayTy s = STUArray s Idx Int
type SorterMonad s = RWST (ArrayTy s) [AnAction] () (ST s)

handleAction :: Action a -> ArrayTy s -> ST s a
handleAction (PeekAt i) arr = readArray arr i
handleAction (CmpAt i j) arr =
    liftA2 compare (readArray arr i) (readArray arr j)
handleAction (SwapAt i j) arr = do
    xi <- readArray arr i
    xj <- readArray arr j
    writeArray arr i xj
    writeArray arr j xi

handleAndLogAction :: Action a -> SorterMonad s a
handleAndLogAction a = do
    tell [AnAction a]
    ask >>= lift . handleAction a

runSort :: (IArray iarr Int)
        => (Idx -> Idx -> Sorter a)       -- Sorting algo
        -> iarr Idx Int                   -- The array
        -> (a, iarr Idx Int, [AnAction])
runSort sorter inAry = runST $ do
    ary <- thaw inAry
    (lo, hi) <- getBounds ary
    let sorting = runSorter handleAndLogAction (sorter lo hi)
    (res, log) <- evalRWST sorting ary ()
    outAry <- freeze ary
    return (res, outAry, log)
