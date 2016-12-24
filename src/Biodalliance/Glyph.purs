module Biodalliance.Glyph
       ( Glyph
       -- , flattenGlyphs
       , Point
       , ScaleFactor
       , VerticalScale
       , linearScale
       , logScale
       , line
       , rect
       , circle
       , GlyphEff
       , GlyphPos
       )
       where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Traversable (traverse_)
import Graphics.Canvas (CANVAS, Context2D)
import Graphics.Canvas as C
import Math as Math

import Biodalliance.Track (TIEREFF)

-- type Glyph a eff = ScaleFactor -> Context2D -> Eff (canvas :: CANVAS | eff) a

type GlyphEff eff = ScaleFactor -> Context2D -> Eff (canvas :: CANVAS | eff) Unit
type GlyphPos = { min :: Unit -> Number
                , max :: Unit -> Number
                , minY :: Unit -> Number
                , maxY :: Unit -> Number
                }

type GlyphEff' eff = ScaleFactor -> Context2D -> Eff (canvas :: CANVAS, tierEff :: TIEREFF | eff) Unit

type Glyph eff = { glyphEff :: GlyphEff eff
                  , glyphPos :: GlyphPos
                  }

sequenceGlyphEffs :: forall eff. Array (GlyphEff eff) -> GlyphEff eff
sequenceGlyphEffs gs sf ctx = traverse_ (\g -> g sf ctx) gs


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
worldToCanvas p sf = { x: ((p.x - sf.viewStart) / sf.bpPerPixel) + 1000.0 -- the BD canvas is offset by 1000px
                    , y: sf.scaleY p.y  }


-- strokeColor :: forall eff. String -> Glyph Context2D eff
-- strokeColor col _ ctx = C.setStrokeStyle col ctx

-- fillColor :: forall eff. String -> Glyph Context2D eff
-- fillColor col _ ctx = C.setFillStyle col ctx

line :: forall eff. Point -> Point -> Glyph eff
line p1 p2 = { glyphEff: lineEff p1 p2
             , glyphPos: linePos p1 p2
             }

lineEff :: forall eff. Point -> Point -> GlyphEff eff
lineEff p1 p2 sf ctx = C.withContext ctx $ do
  let p1' = worldToCanvas p1 sf
      p2' = worldToCanvas p2 sf
  C.moveTo ctx p1'.x p1'.y
  C.lineTo ctx p2'.x p2'.x
  C.stroke ctx
  Track.addGlyph
  pure unit

linePos :: Point -> Point -> GlyphPos
linePos p1 p2 = { min: \_ -> Math.min p1.x p2.x
                , max: \_ -> Math.max p1.x p2.x
                , minY: \_ -> Math.min p1.y p2.y
                , maxY: \_ -> Math.max p1.y p2.y
                }


rect :: forall eff. Point -> Point -> Glyph eff
rect p1 p2 = { glyphEff: rectEff p1 p2
             , glyphPos: rectPos p1 p2
             }

rectEff :: forall eff. Point -> Point -> GlyphEff eff
rectEff p1 p2 sf ctx = C.withContext ctx $ do
  let p1' = worldToCanvas p1 sf
      p2' = worldToCanvas p2 sf
  C.fillRect ctx { x: p1'.x
                 , y: p1'.y
                 , w: p2'.y - p1'.y
                 , h: p2'.y - p1'.y
                 }
  pure unit

rectPos :: Point -> Point -> GlyphPos
rectPos = linePos


circle :: forall eff. Point -> Number -> Glyph eff
circle p r = { glyphEff: circleEff p r
             , glyphPos: circlePos p r
             }

circlePos :: Point -> Number -> GlyphPos
circlePos p r = { min: \_ -> p.x - r
                , max: \_ -> p.x + r
                , minY: \_ -> p.y - r
                , maxY: \_ -> p.y + r
                }

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
