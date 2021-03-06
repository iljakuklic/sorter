# Sorter

Visualizer for all sorts of sorting algorithms written in Haskell.
It uses some interesting implementation techniques detailed further
in the [implementation notes](#implementation-notes) section.

## Setup

Setting up with `stack`:

```
git clone $THIS_REPO
cd sorter/
stack build
```

In the following text, `./sorter` is used as a way of invoking the program.
Replace it with `stack run --` to run it through `stack`.

To check the command line help message:

```
./sorter --help
```

## Examples

Some generated animations with corresponding command lines.

#### Select sort

![Select sort](gifs/select-20-hl.gif)

```
./sorter -s 20 --algorithm select
```

#### Quick sort with specialized sorting networks for partition sizes up to 6

![Quick sort with sortnets](gifs/quick-small-30-hl.gif)

```
./sorter -s 30 --small-nets --algorithm quick
```

#### Bubble sort operating on an array that is already almost in order

![Bubble sort on almost sorted input](gifs/bubble-nearly-30-hl.gif)

```
./sorter -s 30 --nearly-sorted --algorithm bubble
```

## Implementation notes

The implementation of the sorting-algorithm-independent part of the
visualizer consists of three main components. The algorithms themselves
could be considered to be the fourth component.

### The `Sorter` type

The heart of `sorter` is (perhaps surprisingly) the `Sorter` type.
It represents the sorting algorithm. All sorting algorithms are
implemented in terms of this type. `Sorter` is a monad so we can
chain steps of the sorting algorithm and let subsequent steps depend
on results of already performed steps. The `Sorter` provides a number
of basic actions, for example:

```haskell
-- Compare values at two indices
compareAt :: Idx -> Idx -> Sorter Ordering
-- Swap values at two indices
swapAt :: Idx -> Idx -> Sorter ()
```

All operations on the array being sorted have to go through this
interface. The implementer of the sorting algorithm cannot directly
access the array.
Actions are represented explicitly using the `Action` data type.
That means all steps performed by a particular run of the algorithm
to access and manipulate the array can be recorded.

More complicated operations can be defined in terms of these basic
actions, ultimately implementing a sorting algorithm.
The top-level sorting algorithms are passed array boundary indices
(unlike in C, the indices are inclusive).
Example follows:

```haskell
-- Compare two consecutive elements and swap if not in the right order
bubble :: Idx -> Sorter ()
bubble i = do
    c <- compareAt i (i+1)
    when (c == GT) $ swapAt i (i+1)

-- A very simple bubble sort
bubbleSort :: Idx -> Idx -> Sorter ()
bubbleSort begin end =
    for_ [end-1,end-2..begin] $ \stop ->
        for_ [begin..stop] $ \i ->
            bubble i
```

### Sort runner and the animator

The `Sorter` type is a free-monad-style type. Several interpretations
are possible. The `Sorter.Runner` component provides the canonical
interpretation. It keeps track of the contents of the array being sorted
and performs updates to it based on requested actions. In addition to that,
it records all the actions performed during the run of the algorithm
in a log.

Subsequently, the `Sorter.Animator` component inspects the action log
and animates the actions in sequence.

The action logging technique is more general and could be applied to
any free monad. For sake of simplicity, the implementation here
uses the specialized `Sorter` and `Action` types.

### Sorting algorithms

Most sorting algorithms are implemented using open recursion.
That is, a recursive `quicksort` pseudo-code like this:

```haskell
quickSort :: Idx -> Idx -> Sorter ()
quickSort begin end
    | isEmptyOrSingleton begin end = return ()
quickSort begin end = do
    mid <- partition begin end
    quickSort begin (mid-1)
    quickSort (mid+1) end
```

Becomes by replacing the recursive occurrence of `quickSort` by an extra
argument (here called `recur`) this:

```haskell
quickSort :: (Idx -> Idx -> Sorter ()) -> Idx -> Idx -> Sorter ()
quickSort recur begin end
    | isEmptyOrSingleton begin end = return ()
quickSort recur begin end = do
    mid <- partition begin end
    recur begin (mid-1)
    recur (mid+1) end
```

The algorithm proper can then be recovered using `fix quickSort` where
`fix` is the standard fixed point operator from the standard library.

The open recursion style allows us to compose sorting algorithms in various
interesting ways. In particular, there is a combinator that selects the
algorithm depending on array size. For example, a quick sort that switches
to select sort for subranges under 10 elements can be specified as follows:

```haskell
hybridSort :: Idx -> Idx -> Sorter ()
hybridSort = fix $ ifSize (<10) (fix selectSort) . quickSort
```

There is also `sortFocuser` that has the same interface as sorters but does
not sort anything. Instead it only highlights range being sorted in
the animation. This way, we achieve a nice separation of concerns between
visualization and implementing sorting algorithms themselves.
To run bubble sort where the currently sorted range is highlighted, do this:

```haskell
bubbleSortHl :: Idx -> Idx -> Sorter ()
bubbleSortHl = fix $ sortFocuser . bubbleSort
```

## TODO

Random ideas, in no particular order.

* Improvements to quick sort
  * More partitioning schemes
  * Pluggable way to select the pivot
* Visualization improvements
  * Show pivot value
  * Insertion animation
* More sorting algorithms
  * insert sort
  * shell sort
  * heap sort
  * (...)
* Sorter implementation improvements
  * Sorter to use final encoding
  * Expose state to the sorting algorithms (not sure how would that compose?)
* More flexibility specifying input array
  * More options to generate the input randomly
* More flexibility regarding the visualization
  * Allow colours to be adjusted
  * Allow animation speed to be adjusted
  * Allow window size (and GIF size) to be adjusted
* General software engineering improvements
  * Make GIF export a build-time option
    (it is a bit awkward to set up the GIF export library dependency)
