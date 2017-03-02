module Biodalliance.Glyph.Free.Position
       ( glyphToGlyphPosition )
       where

import Prelude
import Biodalliance.Types (Point)
import Biodalliance.Glyph.Free (GlyphF(..), Glyph)
import Biodalliance.Glyph.Position (GlyphPosition(GlyphPos))
import Control.Monad.Free (foldFree)
import Control.Monad.RWS (tell)
import Control.Monad.Writer (Writer, execWriter)
import Math as Math


rectanglePos :: Point -> Point -> GlyphPosition
rectanglePos p1 p2 = GlyphPos ({ min: Math.min p1.x p2.x
                               , max: Math.max p1.x p2.x
                               , minY: Math.min p1.y p2.y
                               , maxY: Math.max p1.y p2.y
                               })

glyphPosN :: GlyphF ~> Writer GlyphPosition
glyphPosN (Stroke _ a) = pure a
glyphPosN (Fill _ a) = pure a
glyphPosN (Circle p r a) = do
  tell (GlyphPos { min: p.x - (r * 1.5)
                 , max: p.x + (r * 1.5)
                 , minY: p.y - (r * 1.5)
                 , maxY: p.y + (r * 1.5)
                 })
  pure a
glyphPosN (Line p1 p2 a) = do
  tell (rectanglePos p1 p2)
  pure a
glyphPosN (Rect p1 p2 a) = do
  tell (rectanglePos p1 p2)
  pure a
  -- TODO fix this.
  -- will need to change Writer to State I think?
glyphPosN (Translate p a) = do
  pure a
glyphPosN (Scale p a) = do
  pure a


glyphToGlyphPosition :: ∀ a. Glyph a -> GlyphPosition
glyphToGlyphPosition = execWriter <<< foldFree glyphPosN
