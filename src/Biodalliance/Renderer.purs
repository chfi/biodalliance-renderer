module Biodalliance.Renderer
       (renderTier
       , drawTier
       , drawFeature
       , Feature(..)
       , Tier(..)
       )
       where

import Prelude

import Control.Monad.Eff.Console
import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)

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

import Data.Foreign (Foreign, F, writeObject, unsafeFromForeign)
import Data.Foreign.Class (class AsForeign, class IsForeign, (.=), readJSON, readProp, write, read)
import Data.Foreign.Index (prop, (!))
import Data.Foreign.Keys (keys)

import Data.Foreign.Generic


  -- TODO
foreign import getTierCanvasContext :: Foreign -> Context2D

-- foreign import drawTierImpl :: forall eff. Context2D -> Tier -> Eff (canvas :: CANVAS, console :: CONSOLE) Unit
foreign import drawTierImpl :: forall eff. Context2D -> Foreign -> F Tier -> Eff (canvas :: CANVAS | eff) Unit -> Unit

foreign import setViewportHeight :: Foreign -> Number -> Unit


newtype Feature = Feature { min :: Number, max :: Number }

instance featureIsForeign :: IsForeign Feature where
  read value = do
    min <- readProp "min" value
    max <- readProp "max" value
    pure $ Feature { min, max }

instance featureAsForeign :: AsForeign Feature where
  write (Feature f) = writeObject [ "min" .= f.min
                                  , "max" .= f.max
                                  ]


instance featureShow :: Show Feature where
  show (Feature f) = "(Feature: min: " <> show f.min <> ", max: " <> show f.max <> ")"


newtype Tier = Tier { ungroupedFeatures :: StrMap (Array Feature) }


parseObject :: forall a. (IsForeign a) => Foreign -> F (Array (Tuple String a))
parseObject f = do
  ks <- keys f
  vs <- traverse (\k -> f ! k >>= read) ks
  pure $ zip ks vs


instance tierIsForeign :: IsForeign Tier where
  read value = do
    features <- readProp "ungroupedFeatures" value >>= parseObject
    let mapped = Map.fromFoldable features
    pure $ Tier { ungroupedFeatures: mapped }

instance tierAsForeign :: AsForeign Tier where
  write (Tier t) = writeObject [ "ungroupedFeatures" .= writeObject features ]
    where features = map (\(Tuple k v) -> k .= v)
                     $ List.toUnfoldable
                     $ Map.toList t.ungroupedFeatures

instance tierShow :: Show Tier where
  show (Tier t) = "tier: " <> show (Map.size t.ungroupedFeatures)



data Track = Track (Array Feature -> Array (Graphics Unit))


type Glyph = Graphics Unit




drawFeature :: Feature -> Graphics Unit
drawFeature (Feature f) = do
  setFillStyle "#FF0000"
  setStrokeStyle "#000000"
  fillRect $ { x: f.min,w: (f.max - f.min), y: 100.0, h: 10.0 }


renderTier :: Fn2 String Foreign Unit
renderTier = mkFn2 \status tier -> runFn1 drawTier tier



f1 :: Feature
f1 = Feature { min: 100.0, max: 200.0}


     -- TODO parse errors should be dealt with before calling this function
-- drawTierImpl :: F Tier -> Graphics Unit
-- drawTierImpl tier = traceAny tier \_ -> drawFeature f1

drawTier :: Fn1 Foreign Unit
drawTier = mkFn1 \tier -> drawTierImpl (getCtx tier) tier (read tier) (runGraphics (getCtx tier) $ drawFeature f1)
  where getCtx = getTierCanvasContext


-- showFeature :: forall eff. Foreign -> Eff (console :: CONSOLE | eff) Unit
-- showFeature f = logShow $ read f :: F Feature
-- showFeature f = logShow $ runExcept $ readJSON """{}"
