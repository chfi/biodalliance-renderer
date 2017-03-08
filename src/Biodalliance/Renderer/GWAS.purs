module Biodalliance.Renderer.GWAS
       ( glyphifyFeatures
       ) where

import Prelude
import Math as Math
import Biodalliance.Feature (Feature(Feature), chrToScreen)
import Biodalliance.Glyph.Free (Glyph, circle, fill, stroke)
import Biodalliance.Glyph.Free.Interpret (writeGlyph)
import Control.Monad.Except (runExcept)
import Control.MonadZero (empty)
import Data.Array (mapMaybe)
import Data.Either (Either(..))
import Data.Foreign (Foreign, F)
import Data.Foreign.Class (readProp)
import Data.Maybe (Maybe(..))

type GWASRow = (score :: Number)
type GWASFeature = Feature GWASRow

type View = { viewStart :: Number
            , scale :: Number
            }

foreign import parseFeatureImpl :: ∀ a. (a -> Maybe a) -> Maybe a -> Foreign -> Maybe GWASFeature

-- TODO: improve errors; Except would be good here
-- however - how to communicate errors to BD?
parseFeature :: Foreign -> Maybe GWASFeature
parseFeature = parseFeatureImpl Just Nothing

glyphifyFeature :: String -> GWASFeature -> Glyph Unit
glyphifyFeature col (Feature f) = do
  stroke col
  fill col
  circle { x: f.min, y: 300.0 + 10.0 * (Math.log f.score / Math.log 10.0) } 3.0

featureToForeign :: View -> GWASFeature -> Foreign
featureToForeign v f =
  let g = glyphifyFeature "#2222dd" (chrToScreen v.scale v.viewStart f)
  in writeGlyph f g

glyphifyFeatures :: View -> Array Foreign -> Array Foreign
glyphifyFeatures v fs = mapMaybe (\f -> (pure <$> featureToForeign v) =<< parseFeature f) fs
