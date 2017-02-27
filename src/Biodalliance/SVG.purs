module Biodalliance.SVG where

import Prelude
import DOM.Node.Document as Document
import DOM.Node.Element as Element
import Control.Monad.Eff (Eff)
import Control.Monad.State (get, put)
import Control.Monad.State.Trans (StateT)
import Control.Monad.Writer (Writer, tell)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.Node (appendChild)
import DOM.Node.Types (Element, elementToNode)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Data.Traversable (intercalate, traverse_)
import Data.Tuple (Tuple(..))

-- createElementSVG :: ∀ eff. String -> Document -> Eff (dom :: DOM | eff) Element
createElementSVG :: ∀ eff. String -> Eff (dom :: DOM | eff) Element
createElementSVG tag = do
  win <- window
  doc <- document win
  Document.createElementNS
    (toNullable $ Just "http://www.w3.org/2000/svg")
    tag
    (htmlDocumentToDocument doc)


type Attribute = Tuple String String

setAttributes :: ∀ eff. Array Attribute -> Element -> Eff (dom :: DOM | eff) Unit
setAttributes attrs ele = traverse_ (\ (Tuple n v) -> Element.setAttribute n v ele) attrs


data SVGElement = SVGElement String (Array Attribute)

data SVGPathOp = SVGPathMoveTo Number Number
               | SVGPathLineTo Number Number
               | SVGPathClose

derive instance eqSVGPathOp :: Eq SVGPathOp

instance showSVGPathOp :: Show SVGPathOp where
  show (SVGPathMoveTo x y) = "M " <> show x <> " " <> show y
  show (SVGPathLineTo x y) = "L " <> show x <> " " <> show y
  show SVGPathClose = "Z"

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
initialSVG = { stroke: "none"
             , fill: "none"
             , strokeWidth: 1.0
             }


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

circle :: Number -> Number -> Number
       -> SVG Unit
circle x y r = do
  cur <- get
  let circle' = [ Tuple "cx" (show x)
                , Tuple "cy" (show y)
                , Tuple "r" (show r)
                ] <> svgToAttribs cur
  tell [SVGElement "circle" circle']

line ::  Number -> Number -> Number -> Number
     -> SVG Unit
line x1 y1 x2 y2 = do
  cur <- get
  -- TODO would be nice to have a better way of building paths...
  -- free monad or similar would work. Really just a writer monad, I guess
  let path = intercalate " " $ show <$> [ SVGPathMoveTo x1 y1
                                        , SVGPathLineTo x2 y2
                                        , SVGPathClose
                                        ]
  let attribs = [ Tuple "d" path
                ] <> svgToAttribs cur
  tell [SVGElement "path" attribs]

rect :: Number -> Number -> Number -> Number
     -> SVG Unit
rect x1 y1 x2 y2 = do
  cur <- get
  let rect' = [ Tuple "x" (show x1)
              , Tuple "y" (show y1)
              , Tuple "width" (show (x2 - x1))
              , Tuple "height" (show (y2 - y1))
              ] <> svgToAttribs cur
  tell [SVGElement "rect" rect']
