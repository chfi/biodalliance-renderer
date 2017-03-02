module Biodalliance.Renderer.GWAS
       -- ( createRenderer
       -- , prepareGlyphs2
       -- ) where
       where

import Prelude
import Biodalliance.Glyph.Free
import Biodalliance.Glyph.Free.Interpret
import Biodalliance.Feature (Feature(..), chrToScreen)
import Data.Foreign (Foreign)


type GWASRow = (score :: Number)
type GWASFeature = Feature GWASRow
-- type GWASGlyph eff = Glyph GWASRow eff


-- type GWASConfig = RendererConfig ()



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

glyphifyFeature :: GWASFeature -> Glyph Unit
glyphifyFeature (Feature f) = do
  stroke "#ff0000"
  fill "#cccccc"
  circle { x: 0.0, y: 20.0 * f.score } 5.0


glyphifyFeatures :: View -> Array GWASFeature -> Array Foreign
glyphifyFeatures v fs = do
  f <- fs
  let f' = chrToScreen (1.0/v.scale) v.viewStart f
  pure $ writeGlyph f (glyphifyFeature f')
