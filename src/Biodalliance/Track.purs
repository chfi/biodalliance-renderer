module Biodalliance.Track
       ( Tier
       , Feature(..)
       , TIEREFF
       , Renderer
       , render
       , canvasContext
       , setHeight
       , scaleFactor
       , features
       , Quant
       , setQuant
       , setGlyphs
       ) where


import Prelude

import Control.Monad.Eff (Eff)
import Graphics.Canvas (CANVAS, Context2D)

import Biodalliance.Glyph (ScaleFactor, VerticalScale, GlyphPos, Glyph)

type Feature r = { min :: Number, max :: Number | r }

type Quant = { min :: Number, max :: Number }

foreign import data Tier :: *
foreign import data TIEREFF :: !
foreign import data Renderer :: *

foreign import render :: forall eff. Eff (canvas :: CANVAS
                                         , tierEff :: TIEREFF | eff) Unit
                      -> Renderer

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

foreign import setGlyphs :: forall eff. Tier -> Array (Glyph eff) -> Eff (tierEff :: TIEREFF | eff) Unit
