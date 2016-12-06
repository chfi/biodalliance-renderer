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

import Data.Array (concat, zip)
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

type Feature = { min :: Number
               , max :: Number
               }

foreign import data Tier :: *
foreign import data Viewport :: *

-- used to run effectful functions: runEff = function(f) { f() };
foreign import runEff :: forall eff. Eff eff Unit -> Eff eff Unit

foreign import getTierCanvasContext :: Tier -> Context2D
foreign import setTierHeight :: Tier -> Number -> Unit
foreign import drawTierImpl :: Context2D -> Tier -> Unit
foreign import getViewport :: Tier -> Viewport

foreign import getFeatures :: Tier -> (Array Feature)

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
  runGraphics (getTierCanvasContext tier) $ sequence_ $ map drawFeature $ getFeatures tier

renderTier :: forall eff. Fn2 String Tier (Eff (canvas :: CANVAS | eff) Unit)
renderTier = mkFn2 \status tier -> runEff $ runFn1 drawTier tier

drawTier :: forall eff. Fn1 Tier (Eff (canvas :: CANVAS | eff) Unit)
drawTier = mkFn1 drawFeatures
