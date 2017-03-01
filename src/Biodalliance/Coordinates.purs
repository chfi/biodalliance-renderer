module Biodalliance.Coordinates
       (  CoordTransform
       , HCoordTransform
       , VCoordTransform
       , VerticalScale
       , linearScale
       , logScale
       , worldToCanvas
       ) where

import Prelude

import Math as Math
import Biodalliance.Types (Point)

type VerticalScale = Number -> Number

type HCoordTransform = { scale :: Number
                       , viewStart :: Number
                       }

type VCoordTransform = { height :: Number
                       -- , scaleY :: VerticalScale
                       }

type VShift = Number -> Number

type CoordTransform = { h :: HCoordTransform
                      , v :: VCoordTransform
                      }


linearScale :: VerticalScale
linearScale y = y

logScale :: VerticalScale
logScale y = (Math.log y / Math.ln10)

             -- y-coordinates are increasing upward
worldToCanvas :: Point -> CoordTransform -> Point
worldToCanvas p ct = { x, y }
  where x = (p.x - ct.h.viewStart) * ct.h.scale
        y = ct.v.height - p.y * ct.v.height
