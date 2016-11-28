module Biodalliance.Renderer
       (renderTier
       , drawTier
       , drawFeature
       , showFeature
       )
       where

import Prelude

import Control.Monad.Eff.Console
import Control.Monad.Eff (Eff)

import Data.Traversable (traverse_)
import Data.Function.Uncurried

import Debug.Trace

import Graphics.Canvas (CANVAS, Context2D)
import Graphics.Canvas.Free

import Data.Foreign (F, writeObject, unsafeFromForeign)
import Data.Foreign.Class (class AsForeign, class IsForeign, (.=), readJSON, readProp, write)

foreign import getTierCanvasContext :: Tier -> Context2D

type Tier = { ungroupedFeatures :: Array Feature }

type Feature = { min :: Number, max :: Number }

-- type Glyph = Eff (canvas :: CANVAS) Unit
type Glyph = Graphics Unit


-- instance featureAsForeign :: AsForeign Feature where
--   write {min, max} = writeObject [ "min" .= min
--                                  , "max" .= max]


showFeature :: Feature -> String
showFeature {min, max} = "min: " <> (show min) <> ", max: " <> (show max)


drawFeature :: Feature -> Glyph
drawFeature f = do
  setFillStyle "#FF0000"
  setStrokeStyle "#000000"
  fillRect $ { x: f.min,w: (f.max - f.min), y: 100.0, h: 10.0 }


renderTier = mkFn2 \x y -> trace y \_ -> runFn1 drawTier y

-- RENDERTIER = traceAny "renderTier" \_ -> mkFn2 \_ t -> runFn1 drawTier t


f :: Feature
f = { min: 100.0, max: 200.0}


drawTier = mkFn1 \x -> trace "drawTier" \_ -> drawFeature f
