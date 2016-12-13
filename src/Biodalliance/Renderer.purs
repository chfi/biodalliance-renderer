module Biodalliance.Renderer
       (renderTier
       , drawTier
       -- , drawFeature
       -- , Feature(..)
       , Tier(..)
       )
       where

import Prelude

import Control.Monad.Eff.Console
import Control.Monad.Eff (Eff)

import Data.Array (concat, zip, zipWith, tail)
import Data.List as List
import Data.Maybe
import Data.Traversable (traverse_, traverse, sequence)
import Data.Foldable
import Data.Function.Uncurried

import Debug.Trace

import Graphics.Canvas (CANVAS, Context2D)
import Graphics.Canvas.Free

  -- features don't necessarily have scores, though...
  -- TODO: need to deal with this in another way in the future (sum type?)
type Feature = { min :: Number
               , max :: Number
               , score :: Number
               }

foreign import data Tier :: *
foreign import data Viewport :: *

-- used to run effectful functions: runEff = function(f) { f() };
foreign import runEff :: forall eff. Eff eff Unit -> Eff eff Unit

foreign import getTierCanvasContext :: Tier -> Context2D
foreign import setTierHeight :: Tier -> Number -> Unit
foreign import getViewport :: Tier -> Viewport

foreign import getFeatures :: Tier -> (Array Feature)

foreign import tierScaleFactor :: Tier -> ScaleFactor


type Point = { x :: Number, y :: Number}

type ScaleFactor = { bpPerPixel :: Number
                   , viewStart :: Number
                   , canvasHeight :: Number
                   }

worldToCanvas :: Point -> ScaleFactor -> Point
worldToCanvas p c = { x: (p.x - c.viewStart) * c.bpPerPixel
                    , y: p.y * c.canvasHeight }

-- something that only requires a scaling factor and offset before it's drawn to canvas
type DrawInst = ScaleFactor -> Graphics Unit

drawLine :: Point -> Point -> DrawInst
drawLine p1 p2 c = do
  let p1' = worldToCanvas p1 c
      p2' = worldToCanvas p2 c
  trace ("drawing at (" <>
         show p1'.x <> ", " <> show p1'.y <> ") to (" <>
         show p2'.x <> ", " <> show p2'.y <> ")") \_ -> moveTo p1'.x p1'.y
  lineTo p1'.x p1'.y
  stroke

drawRect :: Point -> Point -> DrawInst
drawRect p1 p2 c = do
  let p1' = worldToCanvas p1 c
      p2' = worldToCanvas p2 c
  fillRect { x: p1'.x
           , y: p1'.y
           , w: p2'.y - p1'.y
           , h: p2'.y - p1'.y
           }

pointPlot :: Array Feature
          -> Array DrawInst
pointPlot fs = map (\f -> drawRect { x: f.min, y: f.score - 3.9 }
                                   { x: f.max + 200000.0, y: f.score - 4.0}
                   ) fs

linePlot :: Array Feature -> Array DrawInst
linePlot fs = case tail fs of
                   Just fs' -> zipWith (\a b -> drawLine
                                                { x: a.max, y: (a.score - 4.0) / 5.0 }
                                                { x: b.min, y: (b.score - 4.0) / 5.0 }
                                       ) fs fs'
                   Nothing -> []

apScaleFactor :: ScaleFactor -> Array DrawInst -> Array (Graphics Unit)
apScaleFactor sf dis = map (\i -> i sf) dis

drawFeature :: Feature -> Graphics Unit
drawFeature f = do
    setFillStyle "#FF0000"
    setStrokeStyle "#000000"
    let x1 = f.min / 400000.0 + 1000.0
        x2 = f.max / 400000.0 + 1000.0
    fillRect $ { x: x1, w: (x2 - x1), y: 100.0, h: 10.0 }

drawFeatures :: forall eff. Tier -> Eff (canvas :: CANVAS | eff) Unit
drawFeatures tier = do
  let sf = tierScaleFactor tier
  pure $ setTierHeight tier 500.0
  runGraphics (getTierCanvasContext tier) $ do
    setStrokeStyle "#000000"
    sequence_ $ apScaleFactor sf $ linePlot (getFeatures tier)

  -- TODO these should be written directly in javascript, in another file,
  -- to ensure compatibility
renderTier :: forall eff. Fn2 String Tier (Eff (canvas :: CANVAS | eff) Unit)
renderTier = mkFn2 \status tier -> runEff $ runFn1 drawTier tier

drawTier :: forall eff. Fn1 Tier (Eff (canvas :: CANVAS | eff) Unit)
drawTier = mkFn1 drawFeatures
