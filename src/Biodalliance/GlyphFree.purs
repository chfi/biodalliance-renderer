module Biodalliance.GlyphFree
       where

import Prelude
import Biodalliance.SVG as SVG
import Graphics.Canvas as C
import Math as Math
import Biodalliance.Coordinates (Point, CoordTransform)
import Biodalliance.SVG (SVG, SVGElement(..), initialSVG)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Free (Free, foldFree, liftF)
import Control.Monad.State.Trans (runStateT)
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Foreign (Prop(..), toForeign, writeObject, Foreign)
import Data.Generic.Rep (class Generic)
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype, unwrap)
import Global (infinity)
import Graphics.Canvas (Context2D, CANVAS)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary, arbitrary)

type Feature r = { min :: Number, max :: Number | r }

data GlyphF a =
    Circle Point Number a
  | Line Point Point a
  | Rect Point Point a
  | Stroke String a
  | Fill String a

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




         -- TODO: move type into separate module?
         -- In fact,
newtype GlyphPosition = GlyphPos { min :: Number
                                 , max :: Number
                                 , minY :: Number
                                 , maxY :: Number
                                 }

derive instance genericGlyphPosition :: Generic GlyphPosition _
derive instance newtypeGlyphPosition :: Newtype GlyphPosition _
derive instance eqGlyphPosition :: Eq GlyphPosition

instance arbitraryGlyphPosition :: Arbitrary GlyphPosition where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    pure $ GlyphPos { min: a, max: b, minY: c, maxY: d }

instance showGlyphPosition :: Show GlyphPosition where
  show (GlyphPos (gp)) = "{ min: "  <> show gp.min  <>
                         ", max: "  <> show gp.max  <>
                         ", minY: " <> show gp.minY <>
                         ", maxY: " <> show gp.maxY <>
                         " }"


instance semigroupGlyphPosition :: Semigroup GlyphPosition where
  append (GlyphPos (p1)) (GlyphPos (p2)) =
    GlyphPos ({ min: Math.min (p1.min ) (p2.min )
              , max: Math.max (p1.max ) (p2.max )
              , minY: Math.min (p1.minY ) (p2.minY )
              , maxY: Math.max (p1.maxY ) (p2.maxY )
              })


instance monoidGlyphPosition :: Monoid GlyphPosition where
  mempty = GlyphPos { min:  infinity
                    , max:  (-infinity)
                    , minY: infinity
                    , maxY: (-infinity)
                    }


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

interpPos :: ∀ a. Glyph a -> GlyphPosition
interpPos = execWriter <<< foldFree glyphPosN

-- should we use Reader to send the CoordTransform?
-- CoordTransforms are always needed? I think.
  -- depends on what we mean by always!
  -- what does the transform do anyway?
  -- what does it work on?
  -- could we just run it on whatever before we put it into the GlyphF?
  -- should it be attached to GlyphF?


-- TODO: move this (and other interpreters/natural transformations) into module(s)?
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
  pure a
glyphEffN ctx (Line p1 p2 a) = do
  C.moveTo ctx p1.x p1.y
  C.lineTo ctx p2.x p2.y
  C.stroke ctx
  pure a
glyphEffN ctx (Rect p1 p2 a) = do
  C.fillRect ctx { x: p1.x
                 , y: p1.y
                 , w: p2.x - p1.x
                 , h: p2.y - p1.y
                 }
  pure a


interpEff :: ∀ eff. Context2D -> Glyph ~> Eff (canvas :: CANVAS | eff)
interpEff = foldFree <<< glyphEffN


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


glyphLogEffN :: ∀ eff. GlyphF ~> Eff (console :: CONSOLE | eff)
glyphLogEffN (Stroke c a)   = do
  log $ "Set stroke style to " <> c
  pure a
glyphLogEffN (Fill c a)     = do
  log $ "Set fill style to " <> c
  pure a
glyphLogEffN (Circle p r a) = do
  log $ "Drawing circle at (" <> show p.x <> ", " <> show p.y <>
        ") with radius " <> show r <> "."
  pure a
glyphLogEffN (Line p1 p2 a) = do
  log $ "Drawing line from (" <> show p1.x <> ", " <> show p1.y <> ") to " <>
        "(" <> show p2.x <> ", " <> show p2.y <> ")"
  pure a
glyphLogEffN (Rect p1 p2 a) = do
  log $ "Drawing rectangle from (" <> show p1.x <> ", " <> show p1.y <> ") to " <>
        "(" <> show p2.x <> ", " <> show p2.y <> ")"
  pure a


writeGlyph :: ∀ a r. Feature r -> Glyph a -> Foreign
writeGlyph f g = writeObject [ unsafeProp "draw" $ unsafePerformEff <<< d
                             , unsafeProp "min" $ const p.min
                             , unsafeProp "max" $ const p.max
                             , unsafeProp "minY" $ const p.minY
                             , unsafeProp "maxY" $ const p.max
                             , unsafeProp "feature" f
                             , unsafeProp "toSVG" $ unsafePerformEff <<< (\_ -> toSVG)
                             ]
    where p = unwrap $ (execWriter <<< foldFree glyphPosN) g
          d ctx = foldFree (glyphEffN ctx) g
          toSVG = (unsafePartial SVG.renderSVG <<< runSvgEff) g
          unsafeProp :: ∀ x. String -> x -> Prop
          unsafeProp k v = Prop { key: k, value: toForeign v }


interpSvgEff :: GlyphF ~> SVG
interpSvgEff (Stroke c a)  = do
  SVG.setStrokeStyle c
  pure a
interpSvgEff (Fill c a)     = do
  SVG.setFillStyle c
  pure a
interpSvgEff (Circle p r a) = do
  SVG.circle p.x p.y r
  pure a
interpSvgEff (Line p1 p2 a) = do
  -- TODO: lines!
  pure a
interpSvgEff (Rect p1 p2 a) = do
  SVG.rect p1.x p1.y (p2.x - p1.x) (p2.y - p1.y)
  pure a

runSvgEff :: ∀ a. Glyph a -> Array SVGElement
runSvgEff = execWriter <<< (flip runStateT initialSVG) <<< foldFree interpSvgEff
