-- Sorting visualization.

{-# LANGUAGE GADTs #-}

module Sorter.Animate(animateInWindow, animateGif) where

import Sorter.Spec
import Sorter.Runner

import           Data.Array.Base
import qualified Data.List as L
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Export.Gif as G

-- Visual representation of a bar.
data Bar = Bar {
    -- Bar position, standard spacing between bars = 1.0
    barPos :: Float,
    -- Bar height
    barHeight :: Int,
    -- Bar color
    barColor :: G.Color
  } deriving (Show)

-- Draw a single bar.
drawBar :: Bar -> G.Picture
drawBar (Bar x ht c) = G.translate (x + 0.5) 0 rect
  where
    rect = G.color c (G.rectangleUpperSolid 0.7 (fromIntegral ht))

-- Draw a bunch of bars.
-- The resulting image spans the rectangle (-1.0, -1.0) to (1.0, 1.0).
drawBars :: [Bar] -> G.Picture
drawBars bars = G.translate (-1) (-1) picScaled
  where
    maxHt = fromIntegral (maximum (map barHeight bars)) :: Float
    width = fromIntegral (length bars) :: Float
    pic = foldMap drawBar bars
    picScaled = G.scale (2 * recip width) (2 * recip maxHt) pic


-- Animation state.
data AnimState = AnimState {
    -- Progress of the current animation 0.0..1.0
    asCountdown :: Float,
    -- Current order of bars
    _asOrder :: [Idx],
    -- Remaining actions to be performed
    asActions :: [AnAction]
  }

-- Select color, highlighting bars listed in idxs.
highlight :: [Idx] -> Idx -> G.Color
highlight idxs idx = if idx `elem` idxs then G.red else G.orange

-- Draw bars, highlighting bars at specified indices.
drawBarsWithHighlight :: [Idx] -> [Int] -> [Idx] -> G.Picture
drawBarsWithHighlight ord vals hlx
  = drawBars [ Bar (fromIntegral x) h (highlight hlx x) | (x, h) <- zip ord vals ]

-- Draw the current animation frame.
drawState :: [Int] -> AnimState -> G.Picture
drawState vals (AnimState to ord (AnAction (SwapAt i j) : _)) = bars
  where
    -- Bars to draw. The bars currently being swapped go last because we want
    -- them to be drawn at foreground.
    bars = drawBars [ Bar (pos ix) y (hl ix) | (ix, y) <- static ++ swapping ]
    (swapping, static) = L.partition ((`elem` [i, j]) . fst) (zip ord vals)
    lerp t = (1.0 - t) * (fromIntegral i) + t * (fromIntegral j)
    hl = highlight [i, j]
    pos :: Idx -> Float
    pos ix | ix == i = lerp (1.0 - to)
    pos ix | ix == j = lerp to
    pos ix = fromIntegral ix
drawState vals (AnimState _ ord (AnAction (CmpAt i j) : _))
  = drawBarsWithHighlight ord vals [i, j]
drawState vals (AnimState _ ord _)
  = drawBarsWithHighlight ord vals []

-- Draw image and scale it to the viewport.
draw :: (Float, Float) -> [Int] -> AnimState -> G.Picture
draw (sx, sy) vals ste
    = G.scale (0.45 * sx) (0.40 * sy) (drawState vals ste)

-- Duration of various animation states.
actionDuration :: AnAction -> Float
actionDuration (AnAction (PeekAt _)) = 0.01
actionDuration (AnAction (SwapAt i j)) = 0.1 * (max 3 (min dist 10))
  where dist = abs (fromIntegral j - fromIntegral i) :: Float
actionDuration (AnAction (CmpAt _ _)) = 0.1

-- Update bar order according to action.
updateOrder :: AnAction -> [Idx] -> [Idx]
updateOrder (AnAction (SwapAt i j)) = fmap updateIdx
  where
    updateIdx n | n == i = j
    updateIdx n | n == j = i
    updateIdx n = n
updateOrder _act = id

-- Apply an action to the state.
applyAction :: AnimState -> AnimState
applyAction ste@(AnimState _p _ord []) = ste
applyAction (AnimState _p ord (a:acts)) = AnimState 1.0 (updateOrder a ord) acts

-- Update the animation state.
tick :: Float -> AnimState -> AnimState
tick _dt ste@(AnimState _p _ord []) = ste
tick dt ste@(AnimState p _ord (a:_acts)) =
    if aLeft <= 0.0 then sFinal else sCurr
  where
    aDur = actionDuration a
    aLeft = p * aDur - dt
    sNext = applyAction ste
    sFinal = tick (negate aLeft) sNext
    sCurr = ste { asCountdown = aLeft / aDur }

-- Set up the animation.
animSetup :: (Idx -> Idx -> Sorter a) -> [Int] -> AnimState
animSetup sortAlgo elts = initSte
  where
    ary = listArray (Idx 0, Idx (length elts - 1)) elts :: UArray Idx Int
    (_, _, acts) = runSort sortAlgo ary
    initSte = AnimState 1.0 (take (length elts) [0..]) acts

-- Run animation in a window.
animateInWindow :: (Int, Int) -> (Idx -> Idx -> Sorter a) -> [Int] -> IO ()
animateInWindow winSize@(wsx, wsy) sortAlgo elts = do
    let initSte = animSetup sortAlgo elts
    let win = G.InWindow "Sorter" winSize (50, 50)
    let run = G.simulate win (G.greyN 0.15) 50
    run initSte (draw (fromIntegral wsx, fromIntegral wsy) elts) (const tick)

-- Create an animated gif.
animateGif :: (Int, Int) -> FilePath
           -> (Idx -> Idx -> Sorter a) -> [Int] -> IO ()
animateGif imgSize@(isx, isy) fileName sortAlgo elts = do
    let initSte = animSetup sortAlgo elts
    let anim t = draw (fromIntegral isx, fromIntegral isy) elts (tick t initSte)
    let totalDuration = sum (map actionDuration (asActions initSte))
    let gifDelay = 5 :: Int
    let frameDuration = fromIntegral gifDelay / 100.0 :: Float
    let frames = [0.0,frameDuration..(totalDuration+1.5)]
    let run = G.exportPicturesToGif gifDelay G.LoopingForever
    run imgSize (G.greyN 0.15) fileName anim frames
