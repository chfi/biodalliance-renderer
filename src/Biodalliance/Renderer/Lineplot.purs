module Biodalliance.Renderer.Lineplot
       where

import Prelude
import Biodalliance.Glyph.Free as Glyph
import Biodalliance.Feature (Feature(..), chrToScreen)
import Biodalliance.Glyph.Free (Glyph, stroke, path)
import Biodalliance.Glyph.Free.Interpret (writeGlyph)
import Data.Array (tail, zipWith)
import Data.Foreign (Foreign)
import Data.Maybe (Maybe(..))

type LineRow = (score :: Number)
type LineFeature = Feature LineRow


type LinePlotConfig =
  { minScore :: Number
  , maxScore :: Number
  , color :: String
  }

normalizeScore :: LinePlotConfig -> Number -> Number
normalizeScore conf y = ((y - conf.minScore) / (conf.maxScore))

type View = { viewStart :: Number
            , scale :: Number
            }


linePlot :: Array LineFeature -> Glyph Unit
linePlot fs = do
  stroke "#ff0000"
  let ps = (\(Feature f) -> { x: f.min, y: f.max } ) <$> fs
  path ps

glyphifyFeatures :: View -> Array LineFeature -> Glyph Unit
glyphifyFeatures v fs = linePlot fs



qtlPlotConfig :: LinePlotConfig
qtlPlotConfig = { minScore: 3.0
                , maxScore: 5.0
                , color: "#dd0000"
                }
