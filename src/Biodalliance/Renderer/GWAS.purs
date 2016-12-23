module Biodalliance.Renderer.GWAS
       ( renderTier
       , drawTier
       ) where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Function.Uncurried (Fn1, Fn2, mkFn1, mkFn2, runFn1)
import Graphics.Canvas (CANVAS, setStrokeStyle)

import Biodalliance.Glyph (Glyph, circle, flattenGlyphs, logScale)
import Biodalliance.Track (Tier, Feature)
import Biodalliance.Track as Track

type GWASFeature = Feature ( pValue :: Number )

gwasGlyph :: forall eff. GWASFeature -> Glyph Unit eff
gwasGlyph { min, max, pValue } = circle { x: min, y: pValue } 3.0

gwasPlotGlyph :: forall eff. Tier -> Glyph Unit eff
gwasPlotGlyph tier = flattenGlyphs $ map gwasGlyph (Track.features tier)

drawGwasPlot :: forall eff. Tier -> Eff (canvas :: CANVAS | eff) Unit
drawGwasPlot tier = do
  pure $ Track.setHeight tier 500.0
  let sf = Track.scaleFactor tier logScale
      ctx = Track.canvasContext tier
  setStrokeStyle "#222222" ctx
  gwasPlotGlyph tier sf ctx


renderTier :: forall eff. Fn2 String Tier (Eff (canvas :: CANVAS | eff) Unit)
renderTier = mkFn2 \status tier -> Track.runEff $ runFn1 drawTier tier

drawTier :: forall eff. Fn1 Tier (Eff (canvas :: CANVAS | eff) Unit)
drawTier = mkFn1 drawGwasPlot
