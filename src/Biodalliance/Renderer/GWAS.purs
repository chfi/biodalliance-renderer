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

type GWASFeature = Feature ( score :: Number )

gwasGlyph :: forall eff. ScaleFactor -> GWASFeature -> Glyph eff
gwasGlyph sf { min, max, score } = Glyph.circle { x: min, y: score } 3.0 sf

gwasGlyph' :: forall eff. ScaleFactor -> GWASFeature -> {glyph :: Glyph eff, feature :: GWASFeature }
gwasGlyph' sf f = { glyph: Glyph.circle { x: f.min, y: f.score } 3.0 sf
                  , feature: f
                  }

gwasPlotGlyphs :: forall eff. ScaleFactor -> Array GWASFeature -> Array (Glyph eff)
gwasPlotGlyphs sf fs = map (gwasGlyph sf) fs

gwasPlotGlyphs' :: forall eff. ScaleFactor -> Array GWASFeature -> Array ({glyph :: Glyph eff, feature :: GWASFeature})
gwasPlotGlyphs' sf fs = map (gwasGlyph' sf) fs

drawGwasPlot :: GWASConfig
             -> Tier
             -> Eff (canvas :: CANVAS, tierEff :: TIEREFF) Unit
drawGwasPlot config tier = do
  Track.initialize tier
  Track.setHeight tier config.canvasHeight
  sf <- Track.scaleFactor tier logScale
  ctx <- Track.canvasContext tier
  fs <- Track.features tier
  let glyphs = gwasPlotGlyphs' sf fs
  Track.setGlyphs2 tier glyphs
  setStrokeStyle "#222222" ctx
  -- Glyph.drawGlyphs glyphs sf ctx
  Glyph.drawGlyphs (map (\g -> g.glyph) glyphs) sf ctx


type GWASConfig = { canvasHeight :: Number }


createRenderer :: GWASConfig -> Renderer
createRenderer config = { renderTier
                        , drawTier
                        }
  where drawTier = Track.render <<< drawGwasPlot config
        renderTier = mkFn2 \status tier -> drawTier tier
