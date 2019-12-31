
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main(main) where

import Sorter
import SortAlgo

import           System.Random
import           Control.Monad

main :: IO ()
main = randomRIO (30, 50)
         >>= flip replicateM (randomRIO (10, 300))
         >>= animateInWindow (900, 600) (fix $ smallSort . quickSort)
