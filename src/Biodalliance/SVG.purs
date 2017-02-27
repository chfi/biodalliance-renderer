module Biodalliance.SVG where

import Prelude
import Control.Monad.State as State
import DOM.Node.Document as Document
import DOM.Node.Element as Element
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.State (State, get, put)
import Control.Monad.State.Trans (StateT(..))
import Control.Monad.Writer (Writer, tell)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.Node (appendChild)
import DOM.Node.Types (Element, elementToNode, elementToParentNode)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Nullable (toNullable)
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Debug.Trace (traceAny)
import Partial (crash)
import Partial.Unsafe (unsafePartial)


-- createElementSVG :: ∀ eff. String -> Document -> Eff (dom :: DOM | eff) Element
createElementSVG :: ∀ eff. String -> Eff (dom :: DOM | eff) Element
createElementSVG tag = do
  win <- window
  doc <- document win
  Document.createElementNS
    (toNullable $ Just "http://www.w3.org/2000/svg")
    tag
    (htmlDocumentToDocument doc)


-- setAttribute name value element

type Attribute = Tuple String String

setAttributes :: ∀ eff. Array Attribute -> Element -> Eff (dom :: DOM | eff) Unit
setAttributes attrs ele = traverse_ (\ (Tuple n v) -> Element.setAttribute n v ele) attrs

-- the SVG stuff is not stateful, unlike the canvas.
-- thus, to make the two things compatible (the interpreters should basically be interchangeable),
-- I should make the SVG stateful... e.g. it should keep track of stroke and fill colors,
-- as well as translations. and whatever else.

-- so, we get to use StateT!

-- type SVGPath = Array String

data SVGElement = SVGPath (Array String)
                | SVGElement String (Array Attribute)
                | SVGText String


renderSVGElement :: ∀ eff. Partial => SVGElement -> Eff (dom :: DOM | eff) Element
renderSVGElement (SVGElement tag as) = do
  ele <- createElementSVG tag
  setAttributes as ele
  pure ele

renderSVG :: ∀ eff. Partial => Array SVGElement -> Eff (dom :: DOM | eff) Element
renderSVG as = do
  win <- window
  doc <- document win
  root <- createElementSVG "g"
  let parent = elementToNode root
  traverse_ (\ svgEle -> do
                rendered <- renderSVGElement svgEle
                appendChild (elementToNode rendered) parent) as
  pure root

derive instance genericSVGElement :: Generic SVGElement _
derive instance eqSVGElement :: Eq SVGElement
instance showSVGElement :: Show SVGElement where
  show x = genericShow x

type SVGContext = { stroke :: String
                  , fill :: String
                  , strokeWidth :: Number
                  }


type SVG a = StateT SVGContext (Writer (Array SVGElement)) a

svgToAttribs :: SVGContext -> Array Attribute
svgToAttribs svg = [ Tuple "fill" svg.fill
                   , Tuple "stroke" svg.stroke
                   , Tuple "strokeWidth" (show svg.strokeWidth <> "px")
                   ]

initialSVG :: { "stroke" :: String
              , "fill" :: String
              , "strokeWidth" :: Number
              }
initialSVG = { stroke: "#000000"
             , fill: "#000000"
             , strokeWidth: 1.0
             }

modifyStroke :: ∀ r. { stroke :: String | r } -> String -> { stroke :: String | r}
modifyStroke s c = s { stroke = c }
-- so the various commands work on SVGState...

setStrokeStyle :: String -> SVG Unit
setStrokeStyle color = do
  cur <- get
  let cur' = cur { stroke = color }
  put cur'

setFillStyle :: String ->  SVG Unit
setFillStyle color = do
  cur <- get
  let cur' = cur { fill = color }
  put cur'

-- TODO use Point instead...

circle :: ∀ a.
          Number -> Number -> Number
       -> SVG Unit
circle x y r = do
  cur <- get
  let circle' = [ Tuple "cx" (show x)
                , Tuple "cy" (show y)
                , Tuple "r" (show r)
                ] <> svgToAttribs cur
  tell [SVGElement "circle" circle']
  -- put $ cur { elements = cur.elements <> [SVGElement circle'] }



-- to support lines, I need to add Path support... later!
-- line :: ∀ a.
--         Number -> Number -> Number -> Number
     -- -> SVG a
     -- -> SVG ((dom :: DOM | eff) Element)
     -- -> SVG a
-- line _ _ _ _ = do
--   pure mempty
-- line x1 y1 x2 y2 s = do
--   cur <- get
--   ele <- liftEff $ createElementSVG "circle"
--   let attribs = [ Tuple "cx" (show x)
--                 , Tuple "cy" (show y)
--                 , Tuple "r" (show r)
--                 ] <> svgToAttribs cur
--   setAttributes attribs ele
--   pure ele


rect :: ∀ a.
        Number -> Number -> Number -> Number
     -> SVG Unit
rect x1 y1 x2 y2 = do
  cur <- get
  let rect' = [ Tuple "x" (show x1)
              , Tuple "y" (show y1)
              , Tuple "width" (show (x2 - x1))
              , Tuple "height" (show (y2 - y1))
              ] <> svgToAttribs cur
  tell [SVGElement "rect" rect']
  -- put $ cur { elements = cur.elements <> [SVGElement rect'] }
