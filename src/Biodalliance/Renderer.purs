module Biodalliance.Renderer
       where
-- TODO cleanup exports

import Prelude

import Control.Monad.Eff.Console
import Control.Monad.Eff (Eff)

import Data.Array
import Data.Maybe
import Data.Traversable (traverse_, traverse, sequence, sequence_)
import Data.Function.Uncurried

import Debug.Trace

import Graphics.Canvas (CANVAS, Context2D, setStrokeStyle)

import Biodalliance.Glyph (ScaleFactor, Glyph, Point(..), line, flattenGlyphs)

  -- features don't necessarily have scores
  -- TODO: need to deal with this in another way in the future (sum type?)
type Feature = { min :: Number
               , max :: Number
               , score :: Number
               }

foreign import data Tier :: *

-- used to run effectful functions: runEff = function(f) { f() };
foreign import runEff :: forall eff. Eff eff Unit -> Eff eff Unit

foreign import tierCanvasContext :: Tier -> Context2D
foreign import setTierHeight :: Tier -> Number -> Unit

foreign import tierFeatures :: Tier -> (Array Feature)

foreign import tierScaleFactor :: Tier -> ScaleFactor


linePlotGlyph :: forall eff. Tier -> Glyph Unit eff
linePlotGlyph tier = flattenGlyphs $
  case tail (tierFeatures tier) of
    Nothing -> []
                                          -- TODO: origin shift should be dynamic (max(score) - min(score) + C?)
    Just fs' -> zipWith (\f1 f2 -> line { x: f1.max, y: (f1.score - 4.0) / 5.0 }
                                        { x: f2.min, y: (f2.score - 4.0) / 5.0 }
                        ) (tierFeatures tier) fs'



drawFeatures :: forall eff. Tier -> Eff (canvas :: CANVAS | eff) Unit
drawFeatures tier = do
  let sf = tierScaleFactor tier
      ctx = tierCanvasContext tier
  -- TODO: dynamic tier height
  pure $ setTierHeight tier 500.0
  setStrokeStyle "#000000" ctx
  linePlotGlyph tier sf ctx

  -- TODO these should be written directly in javascript, in another file,
  -- to ensure compatibility
renderTier :: forall eff. Fn2 String Tier (Eff (canvas :: CANVAS | eff) Unit)
renderTier = mkFn2 \status tier -> runEff $ runFn1 drawTier tier

drawTier :: forall eff. Fn1 Tier (Eff (canvas :: CANVAS | eff) Unit)
drawTier = mkFn1 drawFeatures
