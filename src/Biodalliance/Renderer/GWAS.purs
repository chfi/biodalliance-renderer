module Biodalliance.Renderer.GWAS
       where
       -- ( createRenderer
       -- ) where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Function.Uncurried (mkFn2)
import Graphics.Canvas (CANVAS, setStrokeStyle)

import Biodalliance.Glyph (Glyph, logScale, ScaleFactor)
import Biodalliance.Glyph as Glyph
import Biodalliance.Track (Tier, Feature, TIEREFF, Renderer)
import Biodalliance.Track as Track

import Biodalliance.Renderer (RendererConfig)

type GWASFeature = Feature ( score :: Number )


-- TODO: clean this up. {glyph :: Glyph eff, feature :: GWASFeature} should have a type synonym.

gwasGlyph :: forall eff. ScaleFactor -> GWASFeature -> {glyph :: Glyph eff, feature :: GWASFeature }
gwasGlyph sf f = { glyph: Glyph.circle { x: f.min, y: f.score } 3.0 sf
                 , feature: f
                 }

gwasPlotGlyphs :: forall eff. ScaleFactor -> Array GWASFeature -> Array ({glyph :: Glyph eff, feature :: GWASFeature})
gwasPlotGlyphs sf fs = map (gwasGlyph sf) fs

drawGwasPlot :: GWASConfig
             -> Tier
             -> Eff (canvas :: CANVAS, tierEff :: TIEREFF) Unit
drawGwasPlot config tier = do
  Track.initialize tier
  Track.prepareViewport tier config.canvasHeight
  sf <- Track.scaleFactor tier logScale
  ctx <- Track.canvasContext tier
  fs <- Track.features tier
  let glyphs = gwasPlotGlyphs sf fs
  Track.setGlyphs tier glyphs
  setStrokeStyle "#222222" ctx
  Glyph.drawGlyphs (map (\g -> g.glyph) glyphs) sf ctx


-- TODO: add type RenderConfig r = { canvasHeight, yOffset | r} to some Renderer module
-- maybe add renderer typeclass??? or is that overkill. would at least give some type-level help,
-- though there's nothing to enforce that renderers actually implement that typeclass.

type GWASConfig = RendererConfig () -- { canvasHeight :: Number }


createRenderer :: GWASConfig -> Renderer
createRenderer config = { renderTier
                        , drawTier
                        }
  where drawTier = Track.render <<< drawGwasPlot config
        renderTier = mkFn2 \status tier -> drawTier tier
