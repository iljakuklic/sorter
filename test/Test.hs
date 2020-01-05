-- Test whether sorting algorithms actually sort.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test(main) where

import Sorter
import SortAlgo

import qualified Data.List as L
import           Data.String

import           Hedgehog
import qualified Hedgehog.Main as H
import qualified Hedgehog.Range as R
import qualified Hedgehog.Gen as G

-- Handful of custom generators.
genInt = G.int (R.exponentialFrom 0 minBound maxBound)
genList = G.list (R.linear 0 300) genInt

-- Check a sorter against reference implementation `Data.List.sort`.
checkSorter :: TestLimit -> SortAlgo -> Property
checkSorter n sorter = withTests n . property $ do
    xs <- forAll genList
    sortUsing sorter xs === L.sort xs

-- Check a heapifier heapifies correctly.
checkHeapifier :: TestLimit -> (Idx -> Idx -> Sorter ()) -> Property
checkHeapifier n heapifier = withTests n . property $ do
    xs <- forAll genList
    let ys = sortUsing heapifier xs
    annotateShow ys
    assert $ and (zipWith (>=) (ys >>= \y -> [y, y]) (drop 1 ys))

-- Define properties to test individual sorting algorithms.
prop_select = checkSorter 500 (fix selectSort)
prop_bubble = checkSorter 200 (fix bubbleSort)
prop_bubblesimple = checkSorter 100 bubbleSortSimple
prop_quick = checkSorter 500 (fix quickSort)
prop_smallquick = checkSorter 500 (fix $ quickSort . smallSort)
prop_heap = checkSorter 500 heapSort
prop_heapify = checkHeapifier 500 heapify

-- Special testing for small sorting networks.
prop_small = withTests 10000 . property $ do
    (l, xs) <- forAll $ do
        let factorial n = product [1..n]
        l <- G.frequency [ (factorial n, return n) | n <- [2..6] ]
        xs <- G.list (R.singleton l) genInt
        return (l, xs)
    classify (fromString $ "sort" <> show l) True
    sortUsing (fix smallSort) xs === L.sort xs

-- Test heap sort that does not start at index 0 by sorting an array prefix
-- with select sort first.
prop_selectheap = withTests 500 . property $ do
    pfxSize <- forAll $ G.int (R.linear 1 50)
    xs <- forAll genList
    let threshold = Idx (length xs - pfxSize)
    let sorter = fix (ifSize (<=threshold) heapSort . selectSort)
    sortUsing sorter xs === L.sort xs

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
