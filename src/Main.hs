-- Main command line interface to sorter

module Main(main) where

import           Sorter
import           SortAlgo
import qualified CmdLine as CL

import           System.Random
import           Control.Monad
import           Text.Read

-- Some sorts are implemented as open sorts, some are closed.
-- We collect any and all of them in ASort.
data ASort = Open OpenSortAlgo | Closed SortAlgo

-- Build a sorting procedure as specified by command line options.
makeSorter :: CL.Options -> SortAlgo
makeSorter opts = mainSorter
  where
    sizeOpt f = Idx (maybe 0 CL.getSize (f opts))
    selectSize = sizeOpt CL.selectSortUpto
    bubbleSize = sizeOpt CL.bubbleThreshold
    smallSpecSorter = if CL.smallNets opts then smallSort else id
    smallSelectSorter = ifSize (<= selectSize) (fix selectSort)
    smallSorter = smallSpecSorter . smallSelectSorter
    baseSorter = case CL.algorithm opts of
        CL.Bubble -> Open bubbleSort
        CL.BubbleSimple -> Closed bubbleSortSimple
        CL.Heap -> Closed heapSort
        CL.Quick -> Open quickSort
        CL.Select -> Open selectSort
    stopSorter = ifSize (<= bubbleSize) noSort
    mainSorter = case baseSorter of
        Open sorter -> \beg end -> do
            fix (stopSorter . sortFocuser . smallSorter . sorter) beg end
            when (bubbleSize > 0) $ fix (sortFocuser . bubbleSort) beg end
        Closed sorter -> sorter

-- Read an input file.
readInput :: CL.File -> IO [Int]
readInput (CL.File f) = either fail return =<< fmap process (readFile f)
  where
    process :: String -> Either String [Int]
    process = sequence . fmap (validate <=< readItem) . words
    readItem = maybe (Left "Inputs must be integers") Right . readMaybe
    validate n | n > 0 = Right n
    validate _ = Left "Inputs must be strictly positive"

-- Generate a random input.
genInput :: CL.Options -> IO [Int]
genInput opts = do
    case CL.randomSeed opts of
        Just (CL.Seed g) -> setStdGen g
        Nothing ->
            getStdGen >>= putStrLn . ("To reproduce pass -R"<>) . show . show
    size <- case CL.arraySize opts of
        Nothing -> randomRIO (30, 100)
        Just s -> return (CL.getSize s)
    if CL.nearlySorted opts
        then fmap (map (max 10) . drop 1 . scanl (+) 30)
                 (replicateM size (randomRIO (-5, 10)))
        else replicateM size (randomRIO (10, 300))

main :: IO ()
main = do
    opts <- CL.getOptions
    initArray0 <- maybe (genInput opts) readInput (CL.input opts)
    let initArray = (if CL.reverse opts then reverse else id) initArray0
    let sorter = makeSorter opts
    case CL.output opts of
        Nothing -> animateInWindow (900, 600) sorter initArray
        Just (CL.File fname) -> animateGif (400, 250) fname sorter initArray
