
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main(main) where

import Sorter
import SortAlgo

import           System.Random
import           Control.Monad

sorter :: SortAlgo
sorter b e = do
    fix (ifSize (<8) noSort . quickSort) b e
    fix bubbleSort b e

main :: IO ()
main = randomRIO (32, 40)
         >>= flip replicateM (randomRIO (10, 300))
         >>= animateInWindow (900, 600) sorter
