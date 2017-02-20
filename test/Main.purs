module Test.Main where

import Prelude

import Control.Monad.Free (foldFree)

import Data.Foreign (Foreign)
import Biodalliance.GlyphFree (Glyph, GlyphPosition, stroke, rect, circle)
import Biodalliance.GlyphFree as GlyphFree

import Test.QuickCheck.Laws (QC)
import Test.QuickCheck.Laws.Data as Data

import Type.Proxy (Proxy(..))

foreign import testGlyphPos :: Foreign -> String


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

main :: ∀ e. QC e Unit
main = do
  checkGlyphPos
  foldFree GlyphFree.glyphLogEffN exGlyph
