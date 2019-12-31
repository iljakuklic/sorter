
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main(main) where

import Sorter
import SortAlgo

import           System.Random
import           Control.Monad

main :: IO ()
main = randomRIO (40, 100)
         >>= flip replicateM (randomRIO (10, 300))
         >>= animateInWindow (900, 600) selectSort
