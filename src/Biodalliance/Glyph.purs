module Biodalliance.Glyph
       ( Glyph
       , Feature
       , drawGlyphs
       , line
       -- , rect
       , circle
       ) where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Traversable (traverse_)
import Graphics.Canvas (CANVAS, Context2D)
import Graphics.Canvas as C
import Math as Math

import Biodalliance.Coordinates (CoordTransform, Point, worldToCanvas)

type Feature r = { min :: Number, max :: Number | r }


-- used by BD to find onscreen position of glyphs, for clicking/hovering callbacks.
-- function names are exactly as they are called in BD
type GlyphPosition = { min :: Unit -> Number
                     , max :: Unit -> Number
                     , minY :: Unit -> Number
                     , maxY :: Unit -> Number
                     }

type GlyphCanvasEffect eff = Context2D -> Eff (canvas :: CANVAS | eff) Unit

newtype Glyph r eff = Glyph { position :: GlyphPosition
                            , canvasEffect :: GlyphCanvasEffect eff
                            , feature :: Feature r
                            }


drawGlyphs :: forall r eff. Array (Glyph r eff) -> GlyphCanvasEffect eff
drawGlyphs gs ctx = traverse_ (\ (Glyph g) -> g.canvasEffect ctx) gs


rectanglePos :: Point -> Point -> GlyphPosition
rectanglePos p1 p2 = { min: \_ -> Math.min p1.x p2.x
                     , max: \_ -> Math.max p1.x p2.x
                     , minY: \_ -> Math.min p1.y p2.y
                     , maxY: \_ -> Math.max p1.y p2.y
                     }


line :: forall r eff.
        Point -> Point
     -> CoordTransform -> Feature r -> Glyph r eff
line p1 p2 ct f = Glyph { canvasEffect, position, feature: f }
  where p1' = worldToCanvas p1 ct
        p2' = worldToCanvas p2 ct

        canvasEffect ctx = C.withContext ctx $ do
          C.moveTo ctx p1'.x p1'.y
          C.lineTo ctx p2'.x p2'.y
          C.stroke ctx
          pure unit

        position = rectanglePos p1' p2'


rect :: forall r eff.
        Point -> Point
     -> CoordTransform -> Feature r -> Glyph r eff
rect p1 p2 ct f = Glyph { canvasEffect, position, feature: f }
  where p1' = worldToCanvas p1 ct
        p2' = worldToCanvas p2 ct

        canvasEffect ctx = C.withContext ctx $ do
          C.fillRect ctx { x: p1'.x
                         , y: p1'.y
                         , w: p2'.x - p1'.x
                         , h: p2'.y - p1'.y
                         }
          pure unit

        position = rectanglePos p1' p2'


circle :: forall r eff. Point -> Number -> CoordTransform -> Feature r -> Glyph r eff
circle p r ct f = Glyph { canvasEffect, position, feature: f }
  where p' = worldToCanvas p ct

        canvasEffect ctx = C.withContext ctx $ do
          C.beginPath ctx
          C.arc ctx { x: p'.x
                    , y: p'.y
                    , r: r
                    , start: 0.0
                    , end: 2.0 * Math.pi
                    }
          C.stroke ctx
          pure unit

        position = { min: \_ -> p'.x - (r * 1.5)
                   , max: \_ -> p'.x + (r * 1.5)
                   , minY: \_ -> p'.y - (r * 1.5)
                   , maxY: \_ -> p'.y + (r * 1.5)
                   }
