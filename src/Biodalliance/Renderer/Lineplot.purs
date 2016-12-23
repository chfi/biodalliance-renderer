module Biodalliance.Renderer.Lineplot
       ( renderTier
       , drawTier
       )
       where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Array (tail, zipWith)
import Data.Maybe (Maybe(..))
import Data.Function.Uncurried (Fn1, Fn2, mkFn1, mkFn2, runFn1)
import Graphics.Canvas (CANVAS, setStrokeStyle)

import Biodalliance.Glyph (Glyph, line, flattenGlyphs)
import Biodalliance.Track (Tier, Feature, tierCanvasContext, runEff,
                           setTierHeight, tierFeatures, tierScaleFactor)


type LineFeature = Feature (score :: Number)

type LinePlotConfig = { minScore :: Number
                      , maxScore :: Number
                      , canvasHeight :: Number
                      , color :: String
                      }

normalizeScore :: LinePlotConfig -> Number -> Number
normalizeScore conf y = ((y - conf.minScore) / (conf.maxScore))

linePlotGlyph :: forall eff. LinePlotConfig -> Tier -> Glyph Unit eff
linePlotGlyph conf tier = flattenGlyphs $
  case tail (tierFeatures tier) of
    Nothing -> []
    Just fs' -> zipWith (\f1 f2 -> line { x: f1.max, y: normalizeScore conf f1.score }
                                        { x: f2.min, y: normalizeScore conf f2.score }
                        ) (tierFeatures tier) fs'

drawLinePlot :: forall eff. LinePlotConfig -> Tier -> Eff (canvas :: CANVAS | eff) Unit
drawLinePlot conf tier = do
  let sf = tierScaleFactor tier
      ctx = tierCanvasContext tier
  pure $ setTierHeight tier conf.canvasHeight
  setStrokeStyle conf.color ctx
  linePlotGlyph conf tier sf ctx


qtlPlotConfig :: LinePlotConfig
qtlPlotConfig = { minScore: 3.0
                , maxScore: 5.0
                , canvasHeight: 400.0
                , color: "#dd0000"}

renderTier :: forall eff. Fn2 String Tier (Eff (canvas :: CANVAS | eff) Unit)
renderTier = mkFn2 \status tier -> runEff $ runFn1 drawTier tier

drawTier :: forall eff. Fn1 Tier (Eff (canvas :: CANVAS | eff) Unit)
drawTier = mkFn1 (drawLinePlot qtlPlotConfig)
