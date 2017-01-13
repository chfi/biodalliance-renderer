module Biodalliance.Renderer.Lineplot
       ( createRenderer
       ) where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Array (tail, zipWith)
import Data.Function.Uncurried (mkFn2)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS, setStrokeStyle)

import Biodalliance.Glyph (Glyph, Feature)
import Biodalliance.Glyph as Glyph
import Biodalliance.Track (Tier, TIEREFF, Renderer)
import Biodalliance.Track as Track

import Biodalliance.Coordinates (linearScale, CoordTransform)

import Biodalliance.Renderer (RendererConfig, trackCoordTransform)


type LineRow = (score :: Number)
type LineFeature = Feature LineRow
type LineGlyph eff = Glyph LineRow eff


type LinePlotConfig =
  RendererConfig ( minScore :: Number
                 , maxScore :: Number
                 , color :: String
                 )

normalizeScore :: LinePlotConfig -> Number -> Number
normalizeScore conf y = ((y - conf.minScore) / (conf.maxScore))


linePlotGlyph :: forall eff. CoordTransform
               -> LinePlotConfig
               -> Array LineFeature
               -> Array (LineGlyph eff)
linePlotGlyph ct conf fs = gs
  where fToPoint f = { x: f.min, y: normalizeScore conf f.score }
        gs = case tail fs of
          Nothing -> []
          Just fs' -> zipWith (\f1 f2 -> Glyph.line (fToPoint f1) (fToPoint f2) ct f1)
                      fs fs'


drawLinePlot :: forall eff. LinePlotConfig
             -> Tier
             -> Eff (canvas :: CANVAS, tierEff :: TIEREFF | eff) Unit
drawLinePlot config tier = do
  Track.prepareViewport tier config.canvasHeight
  ct <- trackCoordTransform config linearScale tier
  ctx <- Track.canvasContext tier
  fs <- Track.features tier
  setStrokeStyle config.color ctx
  let glyphs = linePlotGlyph ct config fs
  Track.setGlyphs tier glyphs
  Glyph.drawGlyphs glyphs ctx


qtlPlotConfig :: LinePlotConfig
qtlPlotConfig = { minScore: 3.0
                , maxScore: 5.0
                , canvasHeight: 400.0
                , yOffset: 0.0
                , color: "#dd0000"}


createRenderer :: LinePlotConfig -> Renderer
createRenderer config = { renderTier
                        , drawTier
                        }
  where drawTier = Track.render <<< drawLinePlot config
        renderTier = mkFn2 \status tier -> drawTier tier
