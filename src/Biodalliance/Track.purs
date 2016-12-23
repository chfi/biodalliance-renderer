module Biodalliance.Track
       ( Tier
       , Feature(..)
       , TIEREFF
       , runEff
       , canvasContext
       , setHeight
       , scaleFactor
       , features
       ) where


import Prelude

import Control.Monad.Eff (Eff)
import Graphics.Canvas (Context2D)

import Biodalliance.Glyph (ScaleFactor, VerticalScale)

type Feature r = { min :: Number, max :: Number | r }



foreign import data Tier :: *
foreign import data TIEREFF :: !
foreign import data TIERGET :: !
foreign import data TIERSET :: !

-- TODO: should this really be here?
-- used to run effectful functions: runEff = function(f) { f() };
foreign import runEff :: forall eff. Eff eff Unit -> Eff eff Unit

-- foreign import canvasContext :: Tier -> Context2D
foreign import canvasContext :: forall eff. Tier
                             -> Eff (tierEff :: TIEREFF | eff) Context2D

foreign import setHeight :: forall eff. Tier
                         -> Number
                         -> Eff (tierEff :: TIEREFF | eff) Unit
-- foreign import setHeight :: Tier -> Number -> Unit

foreign import features :: forall eff r. Tier
                        -> Eff (tierEff :: TIEREFF | eff) (Array (Feature r))

foreign import scaleFactor :: forall eff. Tier
                           -> (Number -> VerticalScale)
                           -> Eff (tierEff :: TIEREFF | eff) ScaleFactor
-- foreign import scaleFactor :: Tier -> (Number -> VerticalScale) -> ScaleFactor

-- foreign import setQuant :: Tier -> Eff (tierEff :: TIEREFF) Unit
