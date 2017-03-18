module Biodalliance
       ( qtlGlyphify
       , gwasGlyphify
       ) where

import Biodalliance.Renderer.GWAS as GWAS
import Biodalliance.Renderer.Lineplot as QTL

qtlGlyphify = QTL.glyphifyFeatures
gwasGlyphify = GWAS.render
