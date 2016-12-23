module Biodalliance.Renderer.GWAS
       ( renderTier
       , drawTier
       ) where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Function.Uncurried (Fn2, mkFn2)
import Graphics.Canvas (CANVAS, setStrokeStyle)

import Biodalliance.Glyph (Glyph, circle, flattenGlyphs, logScale)
import Biodalliance.Track (Tier, Feature, TIEREFF, Renderer)
import Biodalliance.Track as Track


type GWASFeature = Feature ( score :: Number )

gwasGlyph :: forall eff. GWASFeature -> Glyph Unit eff
gwasGlyph { min, max, score } = circle { x: min, y: score } 3.0

gwasPlotGlyph :: forall eff. Array GWASFeature
              -> Glyph Unit eff
gwasPlotGlyph fs = flattenGlyphs $ map gwasGlyph fs

drawGwasPlot :: forall eff. Tier
             -> Eff (canvas :: CANVAS, tierEff :: TIEREFF | eff) Unit
drawGwasPlot tier = do
  Track.setHeight tier 500.0
  sf <- Track.scaleFactor tier logScale
  ctx <- Track.canvasContext tier
  setStrokeStyle "#222222" ctx
  fs <- Track.features tier
  gwasPlotGlyph fs sf ctx

renderTier :: Fn2 String Tier Renderer
renderTier = mkFn2 \status tier -> Track.render $ drawTier tier

drawTier :: forall eff. Tier
         -> (Eff (canvas :: CANVAS, tierEff :: TIEREFF  | eff) Unit)
drawTier = drawGwasPlot
