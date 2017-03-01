module Test.Main where

import Prelude
import Biodalliance.GlyphFree as GlyphFree
import Biodalliance.SVG as SVG
import Test.QuickCheck.Laws.Data as Data
import Biodalliance.GlyphFree (Glyph, GlyphPosition, circle, fill, line, rect, stroke)
import Control.Monad.Eff.Console (log, logShow)
import Control.Monad.Free (foldFree)
import Data.Foreign (Foreign)
import Data.Traversable (traverse, traverse_)
import Debug.Trace (traceAny)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck.Laws (QC)
import Type.Proxy (Proxy(..))

foreign import testGlyphPos :: Foreign -> String
foreign import showGlyphSVG :: Foreign -> Unit
foreign import addGlyphSVG :: Foreign -> Unit


exGlyph :: Glyph Unit
exGlyph = do
  stroke "#000000"
  fill "#aaaaaa"
  rect {x: 0.0, y: 0.0} {x: 20.0, y: 140.0}
  circle {x: 48.0, y: 30.0} 5.356
  stroke "#ff0000"
  line {x: -10.0, y: -10.0} {x: 20.0, y: 35.0}
  fill "#000000"
  rect {x: 0.0, y: 10.0} {x: 400.0, y: 15.0}


bdGlyph :: Foreign
bdGlyph = GlyphFree.writeGlyph {min: 0.0, max: 100.0} exGlyph

checkGlyphPos :: ∀ e. QC e Unit
checkGlyphPos = do
  Data.checkSemigroup prxGlyph
  Data.checkMonoid prxGlyph
  where
    prxGlyph = Proxy :: Proxy GlyphPosition

main :: QC _ Unit
main = do
  checkGlyphPos
  foldFree GlyphFree.glyphLogEffN exGlyph
  let eles = GlyphFree.runSvgEff exGlyph
  traverse_ (log <<< show) eles
  -- pure $ showGlyphSVG bdGlyph
  pure unit
