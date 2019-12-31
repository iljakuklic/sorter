
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main(main) where

import Sorter
import SortAlgo.BubbleSort
import SortAlgo.SelectSort
import SortAlgo.QuickSort

import qualified Graphics.Gloss as G
import           System.Random
import           Data.Array.Base
import           Control.Monad

mkArray :: IArray UArray e => [e] -> UArray Idx e
mkArray elts = listArray (Idx 0, Idx (length elts - 1)) elts

main :: IO ()
main = do
    ary <- randomRIO (40, 100) >>= flip replicateM (randomRIO (10, 300))
    let (_, _, acts) = runSort quickSort (mkArray ary)
    let size = (900, 600)
    let win = G.InWindow "Sorter" size (50, 50)
    let initSte = AnimState {
        asCountdown = 1.0,
        asOrder = take (length ary) [0..],
        asActions = acts
      }
    let run = G.simulate win (G.greyN 0.15) 50
    run initSte (draw size ary) (const tick)
