module Biodalliance.Feature
       ( Feature(..)
       , translateFeature
       , scaleFeature
       ) where

import Prelude
import Biodalliance.Types (Point)
import Data.Newtype (class Newtype)

newtype Feature r = Feature { min :: Number
                            , max :: Number
                            | r
                            }

derive instance newtypeFeature :: Newtype (Feature r) _

translateFeature :: Number -> Feature ~> Feature
translateFeature x (Feature f) = Feature $ f { min = f.min + x
                                             , max = f.max + x
                                             }


-- TODO: is this the right way of doing it?
-- or should we translate to origin, scale, translate back?
-- fix after proper testing in BD, if it's a problem.
scaleFeature :: Number -> Feature ~> Feature
scaleFeature x (Feature f) = Feature $ f { min = f.min * x
                                         , max = f.max * x
                                         }
