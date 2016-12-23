module Biodalliance.Track
       ( Tier
       , Feature(..)
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

type Feature r = { min :: Number, max :: Number | r}

foreign import data Tier :: *

-- TODO: should this really be here?
-- used to run effectful functions: runEff = function(f) { f() };
foreign import runEff :: forall eff. Eff eff Unit -> Eff eff Unit

-- TODO: rename to `canvasContext` etc., encourage qualified import e.g. use Track.canvasContext
foreign import canvasContext :: Tier -> Context2D

foreign import setHeight :: Tier -> Number -> Unit

foreign import features :: forall r. Tier -> (Array (Feature r))

foreign import scaleFactor :: Tier -> (Number -> VerticalScale) -> ScaleFactor
