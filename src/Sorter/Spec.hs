-- This module defines infrastricture for specifying sorting algorithms to
-- be visualized. It defenes a specialized data type (derived as free monad)
-- for expressing an algorithm and a set of actions that could be visualized
-- (such as comparing or swapping two array elements). The idea is that the
-- array to be sorted can only be accessed by means of these actions so
-- everyting that happens to the array can be logged and later visualized.

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Sorter.Spec(Idx(..), Action(..), AnAction(..), Sorter,
                   peekAt, cmpAt, swapAt,
                   runSorter) where

import Data.Array
import Control.Monad
import Control.Applicative

-- A wrapper around Int that represents an index into the array.
-- We use a different type since array valueas are also Ints and it would
-- be rather unpleasant if the two got mixed up.
newtype Idx = Idx { getIdx :: Int } deriving (Eq, Ord, Enum, Show)
deriving instance Num Idx
deriving instance Real Idx
deriving instance Integral Idx
deriving instance Ix Idx

-- Set of actions that an algorithm can perform on an array.
data Action a where
    -- Get value of an element at given index.
    PeekAt :: Idx -> Action Int
    -- Swap elements at two indices.
    SwapAt :: Idx -> Idx -> Action ()
    -- Compare elements at two indices. This may be visualized
    -- differently than just peeking at two elements and comparing.
    CmpAt :: Idx -> Idx -> Action Ordering

deriving instance Show (Action a)

-- Hide the action return type so we can store action log in a list.
data AnAction = forall a . AnAction { getTheAction :: Action a }
deriving instance Show AnAction

-- A free monad specialized for sort actions. This allows us chaining actions
-- and lets subsequent actions depend on results of previous ones.
data Sorter a where
    SPure :: a -> Sorter a
    SBind :: Action a -> (a -> Sorter b) -> Sorter b

instance Functor Sorter where
    fmap f (SPure x) = SPure (f x)
    fmap f (SBind x g) = SBind x (fmap f . g)

instance Applicative Sorter where
    pure = SPure
    SPure f <*> xa = fmap f xa
    SBind c fr <*> xa = SBind c (\r -> fr r <*> xa)

instance Monad Sorter where
    return = pure
    SPure x >>= f = f x
    SBind c fr >>= f = SBind c (\r -> fr r >>= f)

instance Semigroup a => Semigroup (Sorter a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (Sorter a) where
    mempty = return mempty

act :: Action a -> Sorter a
act a = SBind a SPure

-- Conveient action wrappers for specifying algorithms.

peekAt :: Idx -> Sorter Int
peekAt i = act (PeekAt i)

cmpAt :: Idx -> Idx -> Sorter Ordering
cmpAt i j = act (CmpAt i j)

swapAt :: Idx -> Idx -> Sorter ()
swapAt i j = when (i /= j) $ act (SwapAt i j)

-- Run sorter in given monad by providing a handler for individual actions.
runSorter :: Monad m => (forall b . Action b -> m b) -> Sorter a -> m a
runSorter _ (SPure x) = return x
runSorter h (SBind a f) = h a >>= runSorter h . f
