module Biodalliance.Glyph.Free
       ( GlyphF(..)
       , Glyph
       , circle, line, rect, stroke, fill, path
       )
       where

import Prelude
import Biodalliance.Types (Point)
import Control.Monad.Free (Free, liftF)

data GlyphF a =
    Circle Point Number a
  | Line Point Point a
  | Rect Point Point a
  | Stroke String a
  | Fill String a
  | Path (Array Point) a

derive instance functorGlyph :: Functor GlyphF

type Glyph = Free GlyphF


circle :: Point -> Number -> Glyph Unit
circle p r = liftF $ Circle p r unit

line :: Point -> Point -> Glyph Unit
line p1 p2 = liftF $ Line p1 p2 unit

rect :: Point -> Point -> Glyph Unit
rect p1 p2 = liftF $ Rect p1 p2 unit

stroke :: String -> Glyph Unit
stroke c = liftF $ Stroke c unit

fill :: String -> Glyph Unit
fill c = liftF $ Fill c unit

path :: Array Point -> Glyph Unit
path ps = liftF $ Path ps unit
