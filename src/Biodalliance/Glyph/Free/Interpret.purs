module Biodalliance.Glyph.Free.Interpret
       ( writeGlyph )
       where

import Prelude
import Biodalliance.Glyph.Free.Canvas as Canvas
import Biodalliance.Glyph.Free.SVG as SVG
import Biodalliance.Feature (Feature)
import Biodalliance.Glyph.Free (Glyph)
import Biodalliance.Glyph.Free.Position (glyphToGlyphPosition)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Foreign (F, Foreign, Prop(..), renderForeignError, toForeign, writeObject)
import Data.Foreign.Null (writeNull)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (unwrap)


writeGlyph' :: ∀ a r. Maybe (Feature r) -> Glyph a -> Foreign
writeGlyph' f g = writeObject [ unsafeProp "draw" $ unsafePerformEff <<< \ctx -> Canvas.renderGlyph ctx g
                             , unsafeProp "min" $ const p.min
                             , unsafeProp "max" $ const p.max
                             , unsafeProp "minY" $ const p.minY
                             , unsafeProp "maxY" $ const p.maxY
                             , unsafeProp "feature" f'
                             , unsafeProp "toSVG" $ unsafePerformEff <<< \_ -> SVG.renderGlyph g
                             ]
    where p = unwrap $ glyphToGlyphPosition g
          f' = fromMaybe writeNull (toForeign <$> f)
          unsafeProp :: ∀ x. String -> x -> Prop
          unsafeProp k v = Prop { key: k, value: toForeign v }



writeGlyph :: ∀ a r. Maybe (Feature r) -> F (Glyph a) -> Foreign
writeGlyph f fG = case runExcept fG of
  Left errors -> toForeign $ fold $ renderForeignError <$> errors
  Right glyph -> writeGlyph' f glyph
