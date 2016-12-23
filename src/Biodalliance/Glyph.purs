module Biodalliance.Glyph
       ( Glyph
       , flattenGlyphs
       , Point
       , ScaleFactor
       , VerticalScale
       , linearScale
       , logScale
       , line
       , rect
       , circle
       )
       where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Traversable (traverse_)
import Graphics.Canvas (CANVAS, Context2D)
import Graphics.Canvas as C
import Math as Math

type Glyph a eff = ScaleFactor -> Context2D -> Eff (canvas :: CANVAS | eff) a

flattenGlyphs :: forall eff. Array (Glyph Unit eff) -> Glyph Unit eff
flattenGlyphs gs sf ctx = traverse_ (\g -> g sf ctx) gs


type ScaleFactor = { bpPerPixel :: Number
                   , viewStart :: Number
                   , canvasHeight :: Number
                   , scaleY :: VerticalScale
                   }


type VerticalScale = Number -> Number

linearScale :: Number -> VerticalScale
linearScale height y = y * height

logScale :: Number -> VerticalScale
logScale height y = (Math.log y) * height


type Point = { x :: Number, y :: Number}

showPoint :: Point -> String
showPoint p = "(" <> show p.x <> ", " <> show p.y <> ")"

worldToCanvas :: Point -> ScaleFactor -> Point
worldToCanvas p c = { x: ((p.x - c.viewStart) / c.bpPerPixel) + 1000.0 -- the BD canvas is offset by 1000px
                    , y: (p.y * c.canvasHeight)  }


strokeColor :: forall eff. String -> Glyph Context2D eff
strokeColor col _ ctx = C.setStrokeStyle col ctx

fillColor :: forall eff. String -> Glyph Context2D eff
fillColor col _ ctx = C.setFillStyle col ctx

line :: forall eff. Point -> Point -> Glyph Unit eff
line p1 p2 sf ctx = do
  let p1' = worldToCanvas p1 sf
      p2' = worldToCanvas p2 sf
  C.moveTo ctx p1'.x p1'.y
  C.lineTo ctx p2'.x p2'.x
  C.stroke ctx
  pure unit

rect :: forall eff. Point -> Point -> Glyph Unit eff
rect p1 p2 sf ctx = do
  let p1' = worldToCanvas p1 sf
      p2' = worldToCanvas p2 sf
  C.fillRect ctx { x: p1'.x
                 , y: p1'.y
                 , w: p2'.y - p1'.y
                 , h: p2'.y - p1'.y
                 }
  pure unit

circle :: forall eff. Point -> Number -> Glyph Unit eff
circle p r sf ctx = do
  let p' = worldToCanvas p sf
  C.arc ctx { x: p'.x
            , y: p'.y
            , r: r
            , start: 0.0
            , end: Math.pi
            }
  pure unit
