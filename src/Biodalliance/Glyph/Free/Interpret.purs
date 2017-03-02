module Biodalliance.Glyph.Free.Interpret
       ( writeGlyph )
       where

import Prelude
import Biodalliance.Glyph.Free.Canvas as Canvas
import Biodalliance.Glyph.Free.SVG as SVG
import Biodalliance.Glyph.Free (Glyph)
import Biodalliance.Glyph.Free.Position (glyphToGlyphPosition)
import Biodalliance.Feature (Feature)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Foreign (Foreign, Prop(..), toForeign, writeObject)
import Data.Newtype (unwrap)


writeGlyph :: ∀ a r. Feature r -> Glyph a -> Foreign
writeGlyph f g = writeObject [ unsafeProp "draw" $ unsafePerformEff <<< \ctx -> Canvas.renderGlyph ctx g
                             , unsafeProp "min" $ const p.min
                             , unsafeProp "max" $ const p.max
                             , unsafeProp "minY" $ const p.minY
                             , unsafeProp "maxY" $ const p.max
                             , unsafeProp "feature" f
                             , unsafeProp "toSVG" $ unsafePerformEff <<< \_ -> SVG.renderGlyph g
                             ]
    where p = unwrap $ glyphToGlyphPosition g
          unsafeProp :: ∀ x. String -> x -> Prop
          unsafeProp k v = Prop { key: k, value: toForeign v }
