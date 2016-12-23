module Biodalliance.Track
       ( Tier
       , Feature(..)
       , runEff
       , tierCanvasContext
       , setTierHeight
       , tierScaleFactor
       , tierFeatures
       ) where

import Prelude

import Control.Monad.Eff (Eff)

import Graphics.Canvas (Context2D)

import Biodalliance.Glyph (ScaleFactor)

type Feature r = { min :: Number, max :: Number | r}

foreign import data Tier :: *

-- used to run effectful functions: runEff = function(f) { f() };
foreign import runEff :: forall eff. Eff eff Unit -> Eff eff Unit

foreign import tierCanvasContext :: Tier -> Context2D

foreign import setTierHeight :: Tier -> Number -> Unit

foreign import tierFeatures :: forall r. Tier -> (Array (Feature r))

foreign import tierScaleFactor :: Tier -> ScaleFactor
