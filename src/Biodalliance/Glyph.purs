module Biodalliance.Glyph
       ( Glyph
       , Feature
       , GlyphDraw
       , Subtier
       , line
       -- , rect
       , circle
       ) where

import Prelude

import Control.Monad.Eff.Unsafe (unsafePerformEff)

import Graphics.Canvas (Context2D)
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


-- TODO: this function is unsafe, and so doesn't really need the `eff`, but...
-- should be a better way of solving this I think
type GlyphDraw eff = Context2D -> Unit
-- type GlyphDraw eff = Context2D -> Eff (canvas :: CANVAS | eff) Unit


type Subtier r eff = { glyphs :: Array (Glyph r eff)
                     , height :: Number
                     }

-- should have a toSVG function, too...
type Glyph r eff = { draw :: GlyphDraw eff
                   , min :: Unit -> Number
                   , max :: Unit -> Number
                   , minY :: Unit -> Number
                   , maxY :: Unit -> Number
                   , feature :: Feature r
                   }


rectanglePos :: Point -> Point -> GlyphPosition
rectanglePos p1 p2 = { min: \_ -> Math.min p1.x p2.x
                     , max: \_ -> Math.max p1.x p2.x
                     , minY: \_ -> Math.min p1.y p2.y
                     , maxY: \_ -> Math.max p1.y p2.y
                     }


combineGlyph :: ∀ r eff. GlyphDraw eff -> GlyphPosition -> Feature r -> Glyph r eff
combineGlyph c p f = { draw: c
                     , min: p.min
                     , max: p.max
                     , minY: p.minY
                     , maxY: p.maxY
                     , feature: f
                     }


line :: ∀ r eff.
        Point -> Point
     -> CoordTransform -> Feature r -> Glyph r eff
line p1 p2 ct f = combineGlyph draw pos f
  where p1' = worldToCanvas p1 ct
        p2' = worldToCanvas p2 ct

        draw ctx = unsafePerformEff $ C.withContext ctx $ do
          C.moveTo ctx p1'.x p1'.y
          C.lineTo ctx p2'.x p2'.y
          C.stroke ctx
          pure unit

        pos = rectanglePos p1' p2'


rect :: ∀ r eff.
        Point -> Point
     -> CoordTransform -> Feature r -> Glyph r eff
rect p1 p2 ct f = combineGlyph draw pos f
  where p1' = worldToCanvas p1 ct
        p2' = worldToCanvas p2 ct

        draw ctx = unsafePerformEff $ C.withContext ctx $ do
          C.fillRect ctx { x: p1'.x
                         , y: p1'.y
                         , w: p2'.x - p1'.x
                         , h: p2'.y - p1'.y
                         }
          pure unit

        pos = rectanglePos p1' p2'


circle :: ∀ r eff. Point -> Number -> CoordTransform -> Feature r -> Glyph r eff
circle p r ct f = combineGlyph draw pos f
  where p' = worldToCanvas p ct

        draw ctx = unsafePerformEff $ C.withContext ctx $ do
          C.beginPath ctx
          C.arc ctx { x: p'.x
                    , y: p'.y
                    , r: r
                    , start: 0.0
                    , end: 2.0 * Math.pi
                    }
          C.stroke ctx
          pure unit

        pos = { min: \_ -> p'.x - (r * 1.5)
              , max: \_ -> p'.x + (r * 1.5)
              , minY: \_ -> p'.y - (r * 1.5)
              , maxY: \_ -> p'.y + (r * 1.5)
              }
