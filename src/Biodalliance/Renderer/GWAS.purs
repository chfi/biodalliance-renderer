module Biodalliance.Renderer.GWAS
       ( glyphifyFeatures
       ) where

import Prelude
import Biodalliance.Glyph.Free (Glyph, circle, fill, stroke)
import Biodalliance.Glyph.Free.Interpret (writeGlyph)
import Biodalliance.Feature (Feature(Feature), chrToScreen)
import Data.Foreign (Foreign)

type GWASRow = (score :: Number)
type GWASFeature = Feature GWASRow

type View = { viewStart :: Number
            , scale :: Number
            }


glyphifyFeature :: String -> GWASFeature -> Glyph Unit
glyphifyFeature col (Feature f) = do
  stroke col
  fill col
  circle { x: f.min, y: 20.0 * f.score } 5.0


glyphifyFeatures :: View -> Array GWASFeature -> Array Foreign
glyphifyFeatures v fs = do
  f <- fs
  let f1 = chrToScreen v.scale v.viewStart f
  pure $ writeGlyph f (glyphifyFeature "#ff0000" f1)
