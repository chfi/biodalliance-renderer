module Biodalliance.Renderer
       (renderTier
       , drawTier
       -- , drawFeature
       -- , Feature(..)
       , Tier(..)
       )
       where

import Prelude

import Data.Maybe

import Data.Coyoneda (Coyoneda, liftCoyoneda)
import Data.Identity (Identity)

import Control.Monad.Free.Trans (FreeT, liftFreeT)
import Control.Monad.Free (Free, liftF)

import Control.Monad.Eff.Console
import Control.Monad.Eff (Eff)

import Data.Array (concat, zip, zipWith, tail)
import Data.List as List
import Data.Traversable (traverse_, traverse, sequence)
import Data.Foldable
import Data.Function.Uncurried
import Data.Tuple

import Data.StrMap (StrMap)
import Data.StrMap as Map

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


data Point = Point { x :: Number, y :: Number}
type Point' = { x :: Number, y :: Number }

type ScaleFactor = { bpPerPixel :: Number
                   , viewStart :: Number
                   , canvasHeight :: Number
                   }


worldToCanvas :: Number
              -> ScaleFactor
              -> Number
worldToCanvas x c = (x - c.viewStart) / c.bpPerPixel


worldToCanvas' :: Point -> ScaleFactor -> Point
worldToCanvas' (Point p) c = Point  { x: (p.x - c.viewStart) / c.bpPerPixel
                                    , y: p.y * c.canvasHeight }


-- something that only requires a scaling factor and offset efore it's drawn to canvas
type DrawInst = ScaleFactor -> Graphics Unit

moveTo' :: Point -> Graphics Unit
moveTo' (Point p) = moveTo p.x p.y

lineTo' :: Point -> Graphics Unit
lineTo' (Point p) = lineTo p.x p.y

drawLine :: Point -> Point -> DrawInst
drawLine p1 p2 c = do
  let p1' = worldToCanvas' p1 c
      p2' = worldToCanvas' p2 c
  setStrokeStyle "#000000"
  moveTo' p1'
  lineTo' p2'
  stroke

drawRect :: Number -> Number -> Number -> Number -> DrawInst
drawRect x1 y1 x2 y2 c = do
  let x1' = worldToCanvas x1 c
      x2' = worldToCanvas x2 c
  fillRect { x: x1'
           , y: y1 * c.canvasHeight
           , w: x2' - x1'
           , h: (y2-y1) * c.canvasHeight}

pointPlot :: Array Feature
          -> Array DrawInst
pointPlot fs = map (\f -> drawRect f.min (f.score - 3.9) (f.max + 200000.0) (f.score - 4.0)) fs

linePlot :: Array Feature -> Array DrawInst
linePlot fs = case tail fs of
                   Just fs' -> zipWith (\a b -> drawLine
                                                (Point { x: a.max, y: a.score})
                                                (Point { x: b.min, y: b.score})) fs fs'
                   Nothing -> []


applyConfig :: Array DrawInst -> Array (Graphics Unit)
applyConfig dis = map (\i -> i {bpPerPixel: 5000.0 , viewStart: 500.0, canvasHeight: 500.0}) dis


drawFeature :: Feature -> Graphics Unit
drawFeature f = do
    setFillStyle "#FF0000"
    setStrokeStyle "#000000"
    let x1 = f.min / 400000.0 + 1000.0
        x2 = f.max / 400000.0 + 1000.0
    fillRect $ { x: x1, w: (x2 - x1), y: 100.0, h: 10.0 }


drawFeatures :: forall eff. Tier -> Eff (canvas :: CANVAS | eff) Unit
drawFeatures tier = do
  pure $ setTierHeight tier 500.0
  runGraphics (getTierCanvasContext tier) $ sequence_ $ applyConfig $ linePlot (getFeatures tier)


  -- TODO these should be written directly in javascript, in another file,
  -- to ensure compatibility
renderTier :: forall eff. Fn2 String Tier (Eff (canvas :: CANVAS | eff) Unit)
renderTier = mkFn2 \status tier -> runEff $ runFn1 drawTier tier

drawTier :: forall eff. Fn1 Tier (Eff (canvas :: CANVAS | eff) Unit)
drawTier = mkFn1 drawFeatures
