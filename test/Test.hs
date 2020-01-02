-- Test whether sorting algorithms actually sort.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test(main) where

import Sorter
import SortAlgo
import SortAlgo.Algo

import qualified Data.List as L
import           Data.String

import           Hedgehog
import qualified Hedgehog.Main as H
import qualified Hedgehog.Range as R
import qualified Hedgehog.Gen as G

-- Handful of custom generators.
genInt = G.int (R.exponentialFrom 0 minBound maxBound)

-- Check a sorter against reference implementation `Data.List.sort`.
checkSorter :: SortAlgo -> Property
checkSorter sorter = withTests 500 . property $ do
    xs <- forAll $ G.list (R.linear 0 300) genInt
    sortUsing sorter xs === L.sort xs

-- Define properties to test individual sorting algorithms.
prop_select = checkSorter (fix selectSort)
prop_bubble = withTests 200 $ checkSorter (fix bubbleSort)
prop_quick = checkSorter (fix quickSort)
prop_smallquick = checkSorter (fix $ quickSort . smallSort)

-- Special testing for small sorting networks.
prop_small = withTests 10000 . property $ do
    (l, xs) <- forAll $ do
        let factorial n = product [1..n]
        l <- G.frequency [ (factorial n, return n) | n <- [2..6] ]
        xs <- G.list (R.singleton l) genInt
        return (l, xs)
    classify (fromString $ "sort" <> show l) True
    sortUsing (fix smallSort) xs === L.sort xs

-- Test range calculation.
prop_rangesize = property $ do
    i <- forAll $ fmap Idx (G.int (R.linearFrom 0 (-10) 100))
    j <- forAll $ fmap Idx (G.int (R.linear 1 100))
    rangeSize i i === 1
    rangeSize j j === 1
    rangeSize i (i-j) === 0
    rangeSize i (i+j) === j + 1

-- Kick the testing off.
main :: IO ()
main = H.defaultMain [checkSequential $$(discover)]
