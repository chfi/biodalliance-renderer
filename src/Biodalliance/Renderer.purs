module Biodalliance.Renderer
       ( RendererConfig
       ) where

-- TODO: this and many other types could be refactored into a Types module
type RendererConfig c = { canvasHeight :: Number
                        , yOffset :: Number | c }
