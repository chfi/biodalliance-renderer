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

interpSVGEff :: GlyphF ~> SVG
interpSVGEff (Stroke c a)  = do
  SVG.setStrokeStyle c
  pure a
interpSVGEff (Fill c a)     = do
  SVG.setFillStyle c
  pure a
interpSVGEff (Circle p r a) = do
  SVG.circle p.x p.y r
  pure a
interpSVGEff (Line p1 p2 a) = do
  SVG.line p1.x p1.y p2.x p2.y
  pure a
interpSVGEff (Rect p1 p2 a) = do
  SVG.rect p1.x p1.y p2.x p2.y
  pure a

runSVGEff :: ∀ a. Glyph a -> Array SVGElement
runSVGEff = execWriter <<< (flip runStateT initialSVG) <<< foldFree interpSVGEff

renderGlyph :: ∀ a eff. Glyph a -> Eff ( dom :: DOM | eff ) Element
renderGlyph = SVG.renderSVG <<< runSVGEff
