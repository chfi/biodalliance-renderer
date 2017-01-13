module Biodalliance.Coordinates
       ( Point
       , CoordTransform
       , HCoordTransform
       , VCoordTransform
       , VerticalScale
       , linearScale
       , logScale
       , worldToCanvas
       ) where

import Prelude

import Control.Monad.Eff (Eff)

import Math as Math

type Point = { x :: Number, y :: Number}
type VerticalScale = Number -> Number

type HCoordTransform = { bpPerPixel :: Number
                       , viewStart :: Number
                       }

type VCoordTransform = { yOffset :: Number
                       , canvasHeight :: Number
                       , scaleY :: VerticalScale
                       }

type CoordTransform = { h :: HCoordTransform
                      , v :: VCoordTransform
                      }


linearScale :: VerticalScale
linearScale y = y

logScale :: VerticalScale
logScale y = (Math.log y / Math.ln10)

worldToCanvas :: Point -> CoordTransform -> Point
worldToCanvas p ct = { x, y }
  where x = (p.x - ct.h.viewStart) / ct.h.bpPerPixel
        y = ct.v.yOffset + (ct.v.scaleY p.y) * ct.v.canvasHeight
