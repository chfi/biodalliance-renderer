module Biodalliance.Renderer.GWAS
       ( createRenderer
       ) where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Function.Uncurried (mkFn2)
import Graphics.Canvas (CANVAS, setStrokeStyle)

import Biodalliance.Glyph (Glyph, Feature)
import Biodalliance.Glyph as Glyph
import Biodalliance.Track (Tier, TIEREFF, Renderer)
import Biodalliance.Track as Track

import Biodalliance.Coordinates (logScale, CoordTransform)

import Biodalliance.Renderer (RendererConfig, trackCoordTransform)

type GWASRow = (score :: Number)
type GWASFeature = Feature GWASRow
type GWASGlyph eff = Glyph GWASRow eff


type GWASConfig = RendererConfig ()

gwasGlyph :: forall eff. CoordTransform -> GWASFeature -> GWASGlyph eff
gwasGlyph ct f = Glyph.circle { x: f.min, y: f.score } 3.0 ct f


gwasPlotGlyphs :: forall eff. CoordTransform -> Array GWASFeature -> Array (GWASGlyph eff)
gwasPlotGlyphs ct fs = map (gwasGlyph ct) fs


drawGwasPlot :: forall eff. GWASConfig
             -> Tier
             -> Eff (canvas :: CANVAS, tierEff :: TIEREFF | eff ) Unit
drawGwasPlot config tier = do
  Track.initialize tier
  Track.prepareViewport tier config.canvasHeight
  ct <- trackCoordTransform config logScale tier
  ctx <- Track.canvasContext tier
  fs <- Track.features tier
  setStrokeStyle "#222222" ctx
  let glyphs = gwasPlotGlyphs ct fs
  Track.setGlyphs tier glyphs
  Glyph.drawGlyphs glyphs ctx



createRenderer :: GWASConfig -> Renderer
createRenderer config = { renderTier
                        , drawTier
                        }
  where drawTier = Track.render <<< drawGwasPlot config
        renderTier = mkFn2 \status tier -> drawTier tier
