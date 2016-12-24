module Biodalliance.Renderer.GWAS
       ( renderTier
       , drawTier
       ) where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Traversable (traverse_)
import Data.Function.Uncurried (Fn2, mkFn2)
import Graphics.Canvas (CANVAS, setStrokeStyle)

import Biodalliance.Glyph (Glyph, circle, logScale)
import Biodalliance.Track (Tier, Feature, TIEREFF, Renderer)
import Biodalliance.Track as Track


type GWASFeature = Feature ( score :: Number )

gwasGlyph :: forall eff. GWASFeature -> Glyph eff
gwasGlyph { min, max, score } = circle { x: min, y: score } 3.0

gwasPlotGlyphs :: forall eff. Array GWASFeature
              -> Array (Glyph eff)
gwasPlotGlyphs fs = map gwasGlyph fs

drawGwasPlot :: forall eff. Tier
             -> Eff (canvas :: CANVAS, tierEff :: TIEREFF | eff) Unit
drawGwasPlot tier = do
  Track.setHeight tier 500.0
  sf <- Track.scaleFactor tier logScale
  ctx <- Track.canvasContext tier
  setStrokeStyle "#222222" ctx
  fs <- Track.features tier
  -- Track.setQuant tier { min: 1.0, max: 5.0 }
  let glyphs = gwasPlotGlyphs fs
  Track.setGlyphs tier glyphs
  traverse_ (\g -> g.glyphEff sf ctx) glyphs


renderTier :: Fn2 String Tier Renderer
renderTier = mkFn2 \status tier -> drawTier tier

drawTier :: Tier -> Renderer
drawTier = Track.render <<< drawGwasPlot
