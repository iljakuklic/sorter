
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

newtype Idx = Idx { getIdx :: Int } deriving (Eq, Ord, Enum, Show)
deriving instance Num Idx
deriving instance Real Idx
deriving instance Integral Idx
deriving instance Ix Idx

data Action a where
    PeekAt :: Idx -> Action Int
    SwapAt :: Idx -> Idx -> Action ()
    CmpAt :: Idx -> Idx -> Action Ordering

deriving instance Show (Action a)

data AnAction = forall a . AnAction { getTheAction :: Action a }
deriving instance Show AnAction

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

peekAt i = act (PeekAt i)
cmpAt i j = act (CmpAt i j)
swapAt i j = when (i /= j) $ act (SwapAt i j)

runSorter :: Monad m => (forall b . Action b -> m b) -> Sorter a -> m a
runSorter h (SPure x) = return x
runSorter h (SBind a f) = h a >>= runSorter h . f
