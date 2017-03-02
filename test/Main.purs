module Test.Main where

import Prelude
import Biodalliance.Glyph.Free.Canvas as Canvas
import Biodalliance.Glyph.Free.SVG as SVG
import Test.QuickCheck.Laws.Data as Data
import Biodalliance.Glyph.Free (Glyph, circle, fill, line, rect, stroke)
import Biodalliance.Glyph.Free.Interpret (writeGlyph)
import Biodalliance.Glyph.Position (GlyphPosition(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, logShow)
import Control.Monad.Free (foldFree)
import Control.Monad.Maybe.Trans (lift, runMaybeT)
import DOM (DOM)
import DOM.Node.Types (Element)
import Data.Foreign (Foreign)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, traverse_)
import Debug.Trace (traceAny)
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Test.QuickCheck.Laws (QC)
import Type.Proxy (Proxy(..))

foreign import testGlyphPos :: Foreign -> String
foreign import showGlyphSVG :: Foreign -> Unit
foreign import addElementToDiv :: ∀ eff. String -> Element -> Eff ( dom :: DOM | eff ) Unit
foreign import setOnLoad :: ∀ eff. Eff eff Unit -> Eff eff Unit


exGlyph :: Glyph Unit
exGlyph = do
  stroke "#ff0000"
  fill "#333333"
  rect {x: 0.0, y: 0.0} {x: 20.0, y: 140.0}
  stroke "#00ff00"
  fill "#777777"
  circle {x: 48.0, y: 30.0} 5.356
  stroke "#0000ff"
  fill "#aaaaaa"
  rect {x: 0.0, y: 10.0} {x: 400.0, y: 15.0}


bdGlyph :: Foreign
bdGlyph = writeGlyph {min: 0.0, max: 100.0} exGlyph

checkGlyphPosInstances :: ∀ e. QC e Unit
checkGlyphPosInstances = do
  Data.checkSemigroup prxGlyph
  Data.checkMonoid prxGlyph
  where
    prxGlyph = Proxy :: Proxy GlyphPosition

runBrowserTest :: QC _ Unit
runBrowserTest = do
  canvas <- getCanvasElementById "canvas"
  case canvas of
    Nothing -> log "couldn't find canvas"
    Just c  -> do
      ctx <- getContext2D c
      log "rendering glyph to canvas"
      Canvas.renderGlyph ctx exGlyph

  log "rendering glyph to svg"
  SVG.renderGlyph exGlyph >>= addElementToDiv "svgDiv"

  pure unit

main :: QC _ Unit
main = do
  checkGlyphPosInstances
  setOnLoad runBrowserTest
