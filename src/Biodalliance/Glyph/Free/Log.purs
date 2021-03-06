module Biodalliance.Glyph.Free.Log
       ( showGlyph )
       where

import Prelude
import Biodalliance.Glyph.Free (GlyphF(..), Glyph)
import Control.Monad.Free (foldFree)
import Control.Monad.Writer (Writer, execWriter, tell)


glyphLogN :: GlyphF ~> Writer String
glyphLogN (Stroke c a)   = do
  tell $ "Set stroke style to " <> c
  pure a
glyphLogN (Fill c a)     = do
  tell $ "Set fill style to " <> c
  pure a
glyphLogN (Circle p r a) = do
  tell $ "Drawing circle at (" <> show p.x <> ", " <> show p.y <>
         ") with radius " <> show r <> "."
  pure a
glyphLogN (Line p1 p2 a) = do
  tell $ "Drawing line from (" <> show p1.x <> ", " <> show p1.y <> ") to " <>
         "(" <> show p2.x <> ", " <> show p2.y <> ")"
  pure a
glyphLogN (Rect p1 p2 a) = do
  tell $ "Drawing rectangle from (" <> show p1.x <> ", " <> show p1.y <> ") to " <>
         "(" <> show p2.x <> ", " <> show p2.y <> ")"
  pure a


showGlyph :: ∀ a. Glyph a -> String
showGlyph = execWriter <<< foldFree glyphLogN
