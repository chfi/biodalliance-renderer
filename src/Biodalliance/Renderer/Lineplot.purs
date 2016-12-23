module Biodalliance.Renderer.Lineplot
       ( renderTier
       , drawTier
       ) where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Array (tail, zipWith)
import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS, setStrokeStyle)

import Biodalliance.Glyph (Glyph, line, flattenGlyphs, linearScale)
import Biodalliance.Track (Tier, Feature, TIEREFF, Renderer)
import Biodalliance.Track as Track


type LineFeature = Feature (score :: Number)

type LinePlotConfig = { minScore :: Number
                      , maxScore :: Number
                      , canvasHeight :: Number
                      , color :: String
                      }

normalizeScore :: LinePlotConfig -> Number -> Number
normalizeScore conf y = ((y - conf.minScore) / (conf.maxScore))

linePlotGlyph :: forall eff. LinePlotConfig
              -> Array LineFeature
              -> Glyph Unit eff
linePlotGlyph conf fs = flattenGlyphs gs
  where fToPoint f = { x: f.min, y: normalizeScore conf f.score }
        gs = case tail fs of
          Nothing -> []
          Just fs' -> zipWith (\f1 f2 -> line (fToPoint f1) (fToPoint f2))
                      fs fs'


drawLinePlot :: forall eff. LinePlotConfig
             -> Tier
             -> Eff (canvas :: CANVAS, tierEff :: TIEREFF | eff) Unit
drawLinePlot conf tier = do
  Track.setHeight tier conf.canvasHeight
  sf <- Track.scaleFactor tier linearScale
  ctx <- Track.canvasContext tier
  fs <- Track.features tier
  setStrokeStyle conf.color ctx
  linePlotGlyph conf fs sf ctx


qtlPlotConfig :: LinePlotConfig
qtlPlotConfig = { minScore: 3.0
                , maxScore: 5.0
                , canvasHeight: 400.0
                , color: "#dd0000"}

renderTier :: Fn2 String Tier Renderer
renderTier = mkFn2 \status tier -> Track.render $ drawTier tier

drawTier :: forall eff. Tier
         -> (Eff (canvas :: CANVAS, tierEff :: TIEREFF | eff) Unit)
drawTier = (drawLinePlot qtlPlotConfig)
