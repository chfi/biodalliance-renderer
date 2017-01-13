module Biodalliance.Track
       ( Tier
       , TIEREFF
       , hCoordTransform
       , RenderTrack
       , Renderer
       , render
       , initialize
       , canvasContext
       , prepareViewport
       , features
       , Quant
       , setQuant
       , setGlyphs
       ) where


import Prelude

import Control.Monad.Eff (Eff)
import Data.Function.Uncurried (Fn2)
import Graphics.Canvas (CANVAS, Context2D)

import Biodalliance.Glyph (Glyph, Feature)
import Biodalliance.Coordinates (HCoordTransform)

foreign import data RenderTrack :: *


type Renderer = { renderTier :: Fn2 String Tier RenderTrack
                , drawTier :: Tier -> RenderTrack
                }

type Quant = { min :: Number, max :: Number }

foreign import data Tier :: *
foreign import data TIEREFF :: !

foreign import initialize :: forall eff. Tier -> Eff (tierEff :: TIEREFF | eff) Unit

foreign import render :: forall eff. Eff ( canvas :: CANVAS
                                         , tierEff :: TIEREFF | eff) Unit
                      -> RenderTrack

foreign import canvasContext :: forall eff. Tier
                             -> Eff (tierEff :: TIEREFF | eff) Context2D

foreign import prepareViewport :: forall eff. Tier
                               -> Number
                               -> Eff (tierEff :: TIEREFF | eff) Unit

foreign import features :: forall eff r. Tier
                        -> Eff (tierEff :: TIEREFF | eff) (Array (Feature r))


foreign import hCoordTransform :: forall eff. Tier
                               -> Eff (tierEff :: TIEREFF | eff) HCoordTransform


foreign import setQuant :: forall eff. Tier
                        -> Quant
                        -> Eff (tierEff :: TIEREFF | eff) Unit


foreign import setGlyphs :: forall r eff1 eff2.
                            Tier
                         -> Array (Glyph r eff1)
                         -> Eff (tierEff :: TIEREFF | eff2) Unit
