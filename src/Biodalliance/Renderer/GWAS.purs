module Biodalliance.Renderer.GWAS
       ( render
       ) where

import Prelude
import Data.Foreign.Class
import Data.Foreign.Generic
import Data.Generic.Rep
import Math as Math
import Biodalliance.Feature (Feature(Feature), chrToScreen)
import Biodalliance.Glyph.Free (Glyph, circle, fill, stroke)
import Biodalliance.Glyph.Free.Interpret (writeGlyph)
import Control.Error.Util (hush)
import Control.Monad.Except (runExcept)
import Data.Foldable (foldr)
import Data.Foreign (F, Foreign, readString, toForeign)
import Data.Foreign.Index (prop)
import Data.Foreign.Null (writeNull)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Global (readFloat, infinity)

type GWASRow = (score :: Number)
type GWASFeature = Feature GWASRow

readGWASFeature :: Foreign -> F GWASFeature
readGWASFeature f = do
  fMin <- readProp "min" f
  fMax <- readProp "max" f
  score <- prop "pValue" f >>= readString <#> readFloat
  pure $ Feature { min: fMin, max: fMax, score: score }

type View = { viewStart :: Number
            , scale :: Number
            }

glyphifyFeature :: String -> GWASFeature -> Glyph Unit
glyphifyFeature col (Feature f) = do
  stroke col
  fill col
  circle { x: f.min, y: 300.0 + 10.0 * (Math.log f.score / Math.log 10.0) } 3.0

featureToForeign :: View -> F GWASFeature -> Foreign
featureToForeign v f =
  let tf = (chrToScreen v.scale v.viewStart) <$> f
      g = glyphifyFeature "#2222dd" <$> tf
      f' = hush $ runExcept f
  in writeGlyph f' g

writeFeatures :: View -> Array Foreign -> Foreign
writeFeatures v fs = toForeign $ featureToForeign v <<< readGWASFeature <$> fs

quant :: Array GWASFeature -> { min :: Number, max :: Number }
quant = foldr (\(Feature cur) {min, max} -> { min: Math.min cur.score min
                                            , max: Math.max cur.score max})
             { min: infinity, max: (-infinity)}

writeResult :: Foreign -> Maybe { min :: Number, max :: Number } -> Foreign
writeResult g q = toForeign { glyphs: g
                            , quant: q'
                            }
                  where q' = case q of Just x  -> toForeign x
                                       Nothing -> writeNull

render :: View -> Array Foreign -> Foreign
render v fs =
  let fs' = readGWASFeature <$> fs
      gs = toForeign $ featureToForeign v <$> fs'
      q = quant <$> (sequence $ (hush <<< runExcept) <$> fs')
  in writeResult gs q
