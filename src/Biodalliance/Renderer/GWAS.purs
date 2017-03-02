module Biodalliance.Renderer.GWAS
       -- ( createRenderer
       -- , prepareGlyphs2
       -- ) where
       where

import Prelude


import Data.Foldable (minimumBy, maximumBy)
import Data.Maybe (fromMaybe)

-- import Biodalliance.Glyph (Glyph, Feature)
import Biodalliance.Feature (Feature)
import Biodalliance.Glyph.Free as Glyph

import Biodalliance.Coordinates (CoordTransform)

import Biodalliance.Renderer (RendererConfig)

type GWASRow = (score :: Number)
type GWASFeature = Feature GWASRow
-- type GWASGlyph eff = Glyph GWASRow eff


type GWASConfig = RendererConfig ()

-- gwasGlyph :: ∀ eff. CoordTransform -> Number -> Number -> GWASFeature -> GWASGlyph eff
-- gwasGlyph ct min max f = Glyph.circle { x: f.min, y: (f.score / max) } 3.0 ct f


                 -- TODO: need to get the min/max values in here so we can y-scale properly
-- gwasPlotGlyphs :: ∀ eff. CoordTransform -> Array GWASFeature -> Array (GWASGlyph eff)
-- gwasPlotGlyphs ct fs = map (gwasGlyph ct min max) fs
--   where min = fromMaybe 0.0 $ _.score <$> minimumBy ord fs
--         max = fromMaybe 0.0 $ _.score <$> maximumBy ord fs
--         ord f1 f2 = f1.score `compare` f2.score

                    -- TODO: change CoordTransform into this, handle Nothing height and yOffset
type View = { viewStart :: Number
            , scale :: Number
            }

-- glyphifyFeatures :: ∀ eff.
--                     View
--                  -> Array (GWASFeature)
--                  -> Array (GWASGlyph eff)
-- glyphifyFeatures v fs = gwasPlotGlyphs ct fs
--   where ct = { h: { scale: v.scale, viewStart: v.viewStart }
--              , v: { height: 300.0 }
--              }


glyphifyFeatures = id
