module Biodalliance.Glyph
       where
-- TODO cleanup exports

import Prelude

-- import Math as Math

import Control.Monad.Eff.Console
import Control.Monad.Eff (Eff)

import Control.Monad.Reader

import Debug.Trace

import Data.Array (concat, zip, zipWith, tail)
import Data.Identity (Identity)
import Data.Traversable (traverse_, traverse, sequence)

import Graphics.Canvas (CANVAS, Context2D)
import Graphics.Canvas as C


type ScaleFactor = { bpPerPixel :: Number
                   , viewStart :: Number
                   , canvasHeight :: Number
                   }

type Point = { x :: Number, y :: Number}

showPoint :: Point -> String
showPoint p = "(" <> show p.x <> ", " <> show p.y <> ")"

type Glyph a eff = ScaleFactor -> Context2D -> Eff (canvas :: CANVAS | eff) a

-- type Context a = forall eff. Reader { sf :: ScaleFactor, ctx :: Context2D } (Eff (canvas :: CANVAS | eff) a)

flattenGlyphs :: forall eff. Array (Glyph Unit eff) -> Glyph Unit eff
flattenGlyphs gs sf ctx = traverse_ (\g -> g sf ctx) gs

worldToCanvas :: Point -> ScaleFactor -> Point
                                                       -- the BD canvas is offset to the left by 1000px
worldToCanvas p c = { x: ((p.x - c.viewStart) / c.bpPerPixel) + 1000.0
                    , y: (p.y * c.canvasHeight)  }
                    -- , y: p.y }

strokeColor :: forall eff. String -> Glyph Context2D eff
strokeColor col _ ctx = C.setStrokeStyle col ctx

fillColor :: forall eff. String -> Glyph Context2D eff
fillColor col _ ctx = C.setFillStyle col ctx


line :: forall eff. Point -> Point -> Glyph Unit eff
line p1 p2 sf ctx = do
  let p1' = worldToCanvas p1 sf
      p2' = worldToCanvas p2 sf
  trace ("p1': " <> showPoint p1') \_ -> C.moveTo ctx p1'.x p1'.y
  -- C.moveTo ctx p1'.x p1'.y
  trace ("p2': " <> showPoint p2') \_ -> C.lineTo ctx p2'.x p2'.y
  -- C.lineTo ctx p2'.x p2'.x
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
            , end: 3.141592
            }
  pure unit
