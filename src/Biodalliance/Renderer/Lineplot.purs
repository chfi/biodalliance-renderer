module Biodalliance.Renderer.Lineplot
       ( createRenderer
       ) where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Array (tail, zipWith)
import Data.Function.Uncurried (mkFn2)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS, setStrokeStyle)

import Biodalliance.Glyph (Glyph, line, linearScale, ScaleFactor)
import Biodalliance.Glyph as Glyph
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

-- linePlotGlyph :: forall eff. LinePlotConfig
--               -> Array LineFeature
--               -> Array (Glyph eff)
-- linePlotGlyph conf fs = gs
--   where fToPoint f = { x: f.min, y: normalizeScore conf f.score }
--         gs = case tail fs of
--           Nothing -> []
--           Just fs' -> zipWith (\f1 f2 -> line (fToPoint f1) (fToPoint f2))
--                       fs fs'


linePlotGlyph' :: forall eff. ScaleFactor
               -> LinePlotConfig
               -> Array LineFeature
               -> Array ({ glyph :: Glyph eff, feature :: LineFeature })
linePlotGlyph' sf conf fs = gs
  where fToPoint f = { x: f.min, y: normalizeScore conf f.score }
        gs = case tail fs of
          Nothing -> []
          Just fs' -> zipWith (\f1 f2 -> { glyph: line (fToPoint f1) (fToPoint f2) sf, feature: f1})
                      fs fs'

drawLinePlot :: forall eff. LinePlotConfig
             -> Tier
             -> Eff (canvas :: CANVAS, tierEff :: TIEREFF | eff) Unit
drawLinePlot conf tier = do
  Track.prepareViewport tier conf.canvasHeight
  sf <- Track.scaleFactor tier linearScale
  ctx <- Track.canvasContext tier
  fs <- Track.features tier
  setStrokeStyle conf.color ctx
  let glyphs = linePlotGlyph' sf conf fs
  Glyph.drawGlyphs (map (\g -> g.glyph) glyphs) sf ctx


qtlPlotConfig :: LinePlotConfig
qtlPlotConfig = { minScore: 3.0
                , maxScore: 5.0
                , canvasHeight: 400.0
                , color: "#dd0000"}


createRenderer :: LinePlotConfig -> Renderer
createRenderer config = { renderTier
                        , drawTier
                        }
  where drawTier = Track.render <<< drawLinePlot config
        renderTier = mkFn2 \status tier -> drawTier tier
