module Biodalliance.Renderer.Lineplot
       where

import Prelude

import Data.Array (tail, zipWith)
import Data.Maybe (Maybe(..))

import Biodalliance.Types (Feature)
import Biodalliance.Glyph.Free as Glyph

import Biodalliance.Coordinates (CoordTransform)

import Biodalliance.Renderer (RendererConfig)


type LineRow = (score :: Number)
type LineFeature = Feature LineRow


type LinePlotConfig =
  RendererConfig ( minScore :: Number
                 , maxScore :: Number
                 , color :: String
                 )

normalizeScore :: LinePlotConfig -> Number -> Number
normalizeScore conf y = ((y - conf.minScore) / (conf.maxScore))


-- linePlotGlyph :: ∀ eff. CoordTransform
--                -> LinePlotConfig
--                -> Array LineFeature
--                -> Array (LineGlyph eff)
-- linePlotGlyph ct conf fs = gs
--   where fToPoint f = { x: f.min, y: normalizeScore conf f.score }
--         gs = case tail fs of
--           Nothing -> []
--           Just fs' -> zipWith (\f1 f2 -> Glyph.line (fToPoint f1) (fToPoint f2) ct f1)
--                       fs fs'

type View = { viewStart :: Number
            , scale :: Number
            }

-- glyphifyFeatures :: ∀ eff.
--                     View
--                  -> Array (LineFeature)
--                  -> Array (LineGlyph eff)
-- glyphifyFeatures v fs = linePlotGlyph ct qtlPlotConfig fs
--   where ct = { h: { scale: v.scale, viewStart: v.viewStart }
--              , v: { height: 300.0 }
--              }

glyphifyFeatures = id


qtlPlotConfig :: LinePlotConfig
qtlPlotConfig = { minScore: 3.0
                , maxScore: 5.0
                , canvasHeight: 400.0
                , yOffset: 0.0
                , color: "#dd0000"}
