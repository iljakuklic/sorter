
{-# LANGUAGE GADTs #-}

module Sorter.Animate(AnimState(..), draw, tick) where

import Sorter.Spec

import qualified Data.List as L
import qualified Graphics.Gloss as G

data Bar = Bar {
    barPos :: Float,     -- Bar position, standard spacing between bars = 1.0
    barHeight :: Int,    -- Bar height
    barColor :: G.Color  -- Bar color
  } deriving (Show)

drawBar :: Bar -> G.Picture
drawBar (Bar x ht c) = G.translate (x + 0.5) 0 rect
  where
    rect = G.color c (G.rectangleUpperSolid 0.7 (fromIntegral ht))

drawBars :: [Bar] -> G.Picture
drawBars bars = G.translate (-1) (-1) picScaled
  where
    maxHt = fromIntegral (maximum (map barHeight bars)) :: Float
    width = fromIntegral (length bars) :: Float
    pic = foldMap drawBar bars
    picScaled = G.scale (2 * recip width) (2 * recip maxHt) pic

updateOrder :: AnAction -> [Idx] -> [Idx]
updateOrder (AnAction (SwapAt i j)) = fmap updateIdx
  where
    updateIdx n | n == i = j
    updateIdx n | n == j = i
    updateIdx n = n
updateOrder _act = id

data AnimState = AnimState {
    asCountdown :: Float,      -- Progress of the current animation 0.0..1.0
    asOrder :: [Idx],
    asActions :: [AnAction]
  }

highlight idxs idx = if idx `elem` idxs then G.red else G.orange

drawBarsWithHighlight :: [Idx] -> [Int] -> [Idx] -> G.Picture
drawBarsWithHighlight ord vals hlx
  = drawBars [ Bar (fromIntegral x) h (highlight hlx x) | (x, h) <- zip ord vals ]

drawState :: [Int] -> AnimState -> G.Picture
drawState vals ste@(AnimState to ord (AnAction (SwapAt i j) : _)) = bars
  where
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

draw :: (Float, Float) -> [Int] -> AnimState -> G.Picture
draw (sx, sy) vals ste
    = G.scale (0.45 * sx) (0.40 * sy) (drawState vals ste)

actionDuration :: AnAction -> Float
actionDuration (AnAction (PeekAt _)) = 0.01
actionDuration (AnAction (SwapAt i j)) = 0.1 * (max 3 (min dist 10))
  where dist = abs (fromIntegral j - fromIntegral i) :: Float
actionDuration (AnAction (CmpAt _ _)) = 0.1

progressIn a dt = dt / actionDuration a

tick :: Float -> AnimState -> AnimState
tick _dt ste@(AnimState _ _ []) = ste
tick dt ste@(AnimState _ _ (a:acts)) | actionDuration a <= 0.0
  = tick dt (ste { asActions = acts })
tick dt ste@(AnimState p ord (a:acts)) | progressIn a dt < p
  = ste { asCountdown = asCountdown ste - progressIn a dt }
tick dt ste@(AnimState p ord (a:acts))
  = AnimState 1.0 (updateOrder a ord) acts
