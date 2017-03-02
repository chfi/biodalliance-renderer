module Biodalliance.Glyph.Free.Canvas
       ( renderGlyph )
       where


import Prelude
import Graphics.Canvas as C
import Math as Math
import Biodalliance.Glyph.Free (GlyphF(..), Glyph)
import Control.Monad.Eff (Eff)
import Control.Monad.Free (foldFree)
import Graphics.Canvas (CANVAS, Context2D)

glyphEffN :: ∀ eff. Context2D -> GlyphF ~> Eff (canvas :: CANVAS | eff)
glyphEffN ctx (Stroke c a)  = do
  C.setStrokeStyle c ctx
  pure a
glyphEffN ctx (Fill c a)     = do
  C.setFillStyle c ctx
  pure a
glyphEffN ctx (Circle p r a) = do
  C.beginPath ctx
  C.arc ctx { x: p.x
            , y: p.y
            , r: r
            , start: 0.0
            , end: 2.0 * Math.pi
            }
  C.stroke ctx
  C.fill ctx
  pure a
glyphEffN ctx (Line p1 p2 a) = do
  C.moveTo ctx p1.x p1.y
  C.lineTo ctx p2.x p2.y
  C.stroke ctx
  pure a
glyphEffN ctx (Rect p1 p2 a) = do
  let r = { x: p1.x
          , y: p1.y
          , w: p2.x - p1.x
          , h: p2.y - p1.y
          }
  C.fillRect ctx r
  C.strokeRect ctx r
  pure a
glyphEffN ctx (Translate p a) = do
  C.translate { translateX: p.x, translateY: p.y } ctx
  pure a
glyphEffN ctx (Scale p a) = do
  C.scale { scaleX: p.x, scaleY: p.y } ctx
  pure a


renderGlyph :: ∀ eff. Context2D -> Glyph ~> Eff (canvas :: CANVAS | eff)
renderGlyph = foldFree <<< glyphEffN

-- renderGlyph :: ∀ eff. Context2D -> Glyph ~> Eff (canvas :: CANVAS | eff)
-- renderGlyph ctx = foldFree (glyphEffN ctx)
