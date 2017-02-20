module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Control.Monad.Free

import Data.Foreign
import Data.Foreign.Class
import Biodalliance.GlyphFree

foreign import testGlyphPos :: Foreign -> String


exGlyph :: Glyph Unit
exGlyph = do
  stroke "#ffffff"
  rect {x: 0.0, y: 0.0} {x: 20.0, y: 40.0}
  circle {x: 18.0, y: 0.0} 2.356

bdGlyph :: Foreign
bdGlyph = write (BDGlyph { glyph: exGlyph
                         , feature: { min: 0.0, max: 100.0 }
                         })

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  -- log "You should add some tests."
  -- log $ unsafeFromForeign bdGlyph
  -- log $ testGlyphPos bdGlyph
  foldFree glyphLogEffN exGlyph
  -- callDraw bdGlyph
