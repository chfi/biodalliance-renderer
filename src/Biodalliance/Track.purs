module Biodalliance.Track
       ( Tier
       , Feature(..)
       , TIEREFF
       , RenderTrack
       , Renderer
       , initialize
       , render
       , canvasContext
       , setHeight
       , scaleFactor
       , features
       , Quant
       , setQuant
       , setGlyphs
       , setGlyphs2
       ) where


import Prelude

import Control.Monad.Eff (Eff)
import Data.Function.Uncurried (Fn2)
import Graphics.Canvas (CANVAS, Context2D)

import Biodalliance.Glyph (ScaleFactor, VerticalScale, Glyph)
-- import Biodalliance.Renderer (RenderTrack)

foreign import data RenderTrack :: *

type Feature r = { min :: Number, max :: Number | r }

type Renderer = { renderTier :: Fn2 String Tier RenderTrack
                , drawTier :: Tier -> RenderTrack
                }

type Quant = { min :: Number, max :: Number }

foreign import data Tier :: *
foreign import data TIEREFF :: !

foreign import initialize :: forall eff. Tier -> Eff (tierEff :: TIEREFF | eff) Unit

foreign import render :: forall eff. Eff (canvas :: CANVAS
                                         , tierEff :: TIEREFF | eff) Unit
                      -> RenderTrack

foreign import canvasContext :: forall eff. Tier
                             -> Eff (tierEff :: TIEREFF | eff) Context2D

foreign import setHeight :: forall eff. Tier
                         -> Number
                         -> Eff (tierEff :: TIEREFF | eff) Unit

foreign import features :: forall eff r. Tier
                        -> Eff (tierEff :: TIEREFF | eff) (Array (Feature r))

foreign import scaleFactor :: forall eff. Tier
                           -> (Number -> VerticalScale)
                           -> Eff (tierEff :: TIEREFF | eff) ScaleFactor

foreign import setQuant :: forall eff. Tier -> Quant -> Eff (tierEff :: TIEREFF | eff) Unit


foreign import setGlyphs :: forall eff1 eff2. Tier -> Array (Glyph eff1) -> Eff (tierEff :: TIEREFF | eff2) Unit

foreign import setGlyphs2 :: forall eff1 eff2 r.
                             Tier
                          -> Array ({glyph :: Glyph eff1, feature :: Feature r})
                          -> Eff (tierEff :: TIEREFF | eff2) Unit
