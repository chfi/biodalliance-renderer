module Biodalliance.Glyph
       -- ( Glyph
       -- , flattenGlyphs
       -- , Point
       -- , ScaleFactor
       -- , VerticalScale
       -- , linearScale
       -- , logScale
       -- , line
       -- , rect
       -- , circle
       -- , circleEff
       -- , GlyphEff
       -- , GlyphPos
       -- , drawGlyphs
       -- )
       where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Traversable (traverse_)
import Graphics.Canvas (CANVAS, Context2D)
import Graphics.Canvas as C
import Math as Math



type GlyphPos = { min :: Unit -> Number
                , max :: Unit -> Number
                , minY :: Unit -> Number
                , maxY :: Unit -> Number
                }

type GlyphEff eff = ScaleFactor -> Context2D -> Eff (canvas :: CANVAS | eff) Unit

type Glyph eff = { glyphEff :: GlyphEff eff
                 , glyphPos :: GlyphPos
                 }

drawGlyphs :: forall eff. Array (Glyph eff) -> GlyphEff eff
drawGlyphs gs sf ctx = traverse_ (\g -> g.glyphEff sf ctx) gs


type ScaleFactor = { bpPerPixel :: Number
                   , viewStart :: Number
                   , canvasHeight :: Number
                   , scaleY :: VerticalScale
                   }


type VerticalScale = Number -> Number

linearScale :: Number -> VerticalScale
linearScale height y = y * height

logScale :: Number -> VerticalScale
logScale height y = (Math.log y / Math.ln10) * height


type Point = { x :: Number, y :: Number}

showPoint :: Point -> String
showPoint p = "(" <> show p.x <> ", " <> show p.y <> ")"

worldToCanvas :: Point -> ScaleFactor -> Point
worldToCanvas p sf = { x: ((p.x - sf.viewStart) / sf.bpPerPixel)
                    , y: sf.scaleY p.y  }


line :: forall eff. Point -> Point -> ScaleFactor -> Glyph eff
line p1 p2 sf = { glyphEff: lineEff p1 p2
                , glyphPos: linePos p1 p2 sf
                }

lineEff :: forall eff. Point -> Point -> GlyphEff eff
lineEff p1 p2 sf ctx = C.withContext ctx $ do
  let p1' = worldToCanvas p1 sf
      p2' = worldToCanvas p2 sf
  C.moveTo ctx p1'.x p1'.y
  C.lineTo ctx p2'.x p2'.y
  C.stroke ctx
  pure unit

linePos :: Point -> Point -> ScaleFactor -> GlyphPos
linePos p1 p2 sf = { min: \_ -> Math.min p1'.x p2'.x
                   , max: \_ -> Math.max p1'.x p2'.x
                   , minY: \_ -> Math.min p1'.y p2'.y
                   , maxY: \_ -> Math.max p1'.y p2'.y
                   }
  where p1' = worldToCanvas p1 sf
        p2' = worldToCanvas p2 sf


rect :: forall eff. Point -> Point -> ScaleFactor -> Glyph eff
rect p1 p2 sf = { glyphEff: rectEff p1 p2
                , glyphPos: rectPos p1 p2 sf
                }

rectEff :: forall eff. Point -> Point -> GlyphEff eff
rectEff p1 p2 sf ctx = C.withContext ctx $ do
  let p1' = worldToCanvas p1 sf
      p2' = worldToCanvas p2 sf
  C.fillRect ctx { x: p1'.x
                 , y: p1'.y
                 , w: p2'.x - p1'.x
                 , h: p2'.y - p1'.y
                 }
  pure unit

rectPos :: Point -> Point -> ScaleFactor -> GlyphPos
rectPos = linePos


circle :: forall eff. Point -> Number -> ScaleFactor -> Glyph eff
circle p r sf = { glyphEff: circleEff p r
                , glyphPos: circlePos p r sf
                }

circlePos :: Point -> Number -> ScaleFactor -> GlyphPos
circlePos p r sf = { min: \_ -> p'.x - (r * 1.5)
                   , max: \_ -> p'.x + (r * 1.5)
                   , minY: \_ -> p'.y - (r * 1.5)
                   , maxY: \_ -> p'.y + (r * 1.5)
                   }
  where p' = worldToCanvas p sf

circleEff :: forall eff. Point -> Number -> GlyphEff eff
circleEff p r sf ctx = C.withContext ctx $ do
  let p' = worldToCanvas p sf
  C.beginPath ctx
  C.arc ctx { x: p'.x
            , y: p'.y
            , r: r
            , start: 0.0
            , end: 2.0 * Math.pi
            }
  C.stroke ctx
  pure unit
