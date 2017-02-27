module Test.Main where

import Prelude
import Biodalliance.GlyphFree as GlyphFree
import Biodalliance.SVG as SVG
import Test.QuickCheck.Laws.Data as Data
import Biodalliance.GlyphFree (Glyph, GlyphPosition, stroke, rect, circle)
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


exGlyph :: Glyph Unit
exGlyph = do
  stroke "#ffffff"
  rect {x: 0.0, y: 0.0} {x: 20.0, y: 40.0}
  circle {x: 18.0, y: 0.0} 2.356

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
  pure $ showGlyphSVG bdGlyph
  -- eles' <- traverse (unsafePartial SVG.renderSVGElement) eles
  -- traceAny eles' (\_ -> pure unit)
  -- traverse_ (traceAny eles' (\_ -> unit)) eles'
  pure unit
  -- traverse_ (log <<< show <<< SVG.renderSVGElement) eles
