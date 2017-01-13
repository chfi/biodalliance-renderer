module Biodalliance.Renderer
       ( RendererConfig
       , trackCoordTransform
       ) where

import Prelude

import Control.Monad.Eff (Eff)

import Biodalliance.Coordinates (CoordTransform, VerticalScale)

import Biodalliance.Track as Track
import Biodalliance.Track (Tier, TIEREFF)


type RendererConfig c = { canvasHeight :: Number
                        , yOffset :: Number | c }

-- TODO: this is a bad place for this function to be,
-- but it can't go in BD.Coordinates due to
-- dependencies (and that module should be entirely pure)
trackCoordTransform :: forall c eff. RendererConfig c
               -> VerticalScale
               -> Tier
               -> Eff (tierEff :: TIEREFF | eff) CoordTransform
trackCoordTransform rc vs t = do
  hct <- Track.hCoordTransform t
  let vct = { yOffset: rc.yOffset
            , canvasHeight: rc.canvasHeight
            , scaleY: vs
            }

  pure { h: hct, v: vct }
