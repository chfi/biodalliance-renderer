module Biodalliance.Glyph.Free.SVG
       ( renderGlyph )
       where

import Prelude
import Biodalliance.SVG as SVG
import Biodalliance.Glyph.Free (Glyph, GlyphF(..))
import Biodalliance.SVG (SVG, SVGElement, initialSVG)
import Control.Monad.Eff (Eff)
import Control.Monad.Free (foldFree)
import Control.Monad.State (runStateT)
import Control.Monad.Writer (execWriter)
import DOM (DOM)
import DOM.Node.Types (Element)

interpSvgEff :: GlyphF ~> SVG
interpSvgEff (Stroke c a)  = do
  SVG.setStrokeStyle c
  pure a
interpSvgEff (Fill c a)     = do
  SVG.setFillStyle c
  pure a
interpSvgEff (Circle p r a) = do
  SVG.circle p.x p.y r
  pure a
interpSvgEff (Line p1 p2 a) = do
  SVG.line p1.x p1.y p2.x p2.y
  pure a
interpSvgEff (Rect p1 p2 a) = do
  SVG.rect p1.x p1.y p2.x p2.y
  pure a

runSvgEff :: ∀ a. Glyph a -> Array SVGElement
runSvgEff = execWriter <<< (flip runStateT initialSVG) <<< foldFree interpSvgEff

renderGlyph :: ∀ a eff. Glyph a -> Eff ( dom :: DOM | eff ) Element
renderGlyph = SVG.renderSVG <<< runSvgEff
