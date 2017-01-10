module Biodalliance.Renderer
       ( RendererConfig
       ) where

import Prelude

type RendererConfig c = { canvasHeight :: Number
                        , yOffset :: Number | c }


drawPlot :: forall c f eff.
            (Array (Feature f) -> Array ({ glyph :: Glyph eff, feature :: Feature f }))
         -> RendererConfig c
         -> VerticalScale
         -> Tier
         -> Eff (canvas :: CANVAS, tierEff :: TIEREFF) Unit
drawPlot glyphF scale config tier = do
  Track.initialize tier
  Track.prepareViewport config.canvasHeight
  sf <- Track.scaleFactor tier scale
  ctx <- Track.canvasContext tier
  fs <- Track.features tier
  let glyphs = glyphF sf fs
  Track.setGlyphs tier glyphs
  Glyph.drawGlyphs
