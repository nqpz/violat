{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Violat.SDL
-- License     :  WTFPL 2.0
--
-- Stability   :  provisional
-- Portability :  portable
--
-- SDL additions.
--
-----------------------------------------------------------------------------
module Violat.SDL where

#include "violat.h"

import Prelude hiding ((.), id)
import Control.Category
import qualified Data.Label as L
import Control.Monad.State
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDL
import qualified Graphics.UI.SDL.Rotozoomer as SDL
import Data.Ratio
import Control.Monad.Trans.Maybe
import Data.Maybe

import Violat.Types
import Violat.Helpers
import Violat.Graphics
import Violat.Color

i n = fromIntegral n

fillRect r c s = liftIO . SDL.fillRect s r =<< colorToPixel c
pixel x y c s = liftIO . SDL.pixel s (i x) (i y) =<< colorToPixel c
hLine x1 x2 y c s = liftIO . SDL.hLine s (i x1) (i x2) (i y) =<< colorToPixel c
vLine x y1 y2 c s = liftIO . SDL.vLine s (i x) (i y1) (i y2) =<< colorToPixel c
-- rectangle r c s = liftIO . SDL.rectangle s r =<< colorToPixel c
-- box r c s = liftIO . SDL.box s r =<< colorToPixel c
line x y x' y' c s = liftIO . SDL.line s (i x) (i y) (i x') (i y') =<< colorToPixel c
aaLine x y x' y' c s = liftIO . SDL.aaLine s (i x) (i y) (i x') (i y') =<< colorToPixel c
circle x y r c s = liftIO . SDL.circle s (i x) (i y) (i r) =<< colorToPixel c
arc x y r s e c surf = liftIO . SDL.arc surf (i x) (i y) (i r) (i s) (i e) =<< colorToPixel c
aaCircle x y r c s = liftIO . SDL.aaCircle s (i x) (i y) (i r) =<< colorToPixel c
filledCircle x y r c s = liftIO . SDL.filledCircle s (i x) (i y) (i r) =<< colorToPixel c
ellipse x y rx ry c s = liftIO . SDL.ellipse s (i x) (i y) (i rx) (i ry) =<< colorToPixel c
aaEllipse x y rx ry c s = liftIO . SDL.aaEllipse s (i x) (i y) (i rx) (i ry) =<< colorToPixel c
filledEllipse x y rx ry c s = liftIO . SDL.filledEllipse s (i x) (i y) (i rx) (i ry) =<< colorToPixel c
pie x y r s e c surf = liftIO . SDL.pie surf (i x) (i y) (i r) (i s) (i e) =<< colorToPixel c
filledPie x y r s e c surf = liftIO . SDL.filledPie surf (i x) (i y) (i r) (i s) (i e) =<< colorToPixel c
trigon x1 y1 x2 y2 x3 y3 c s = liftIO . SDL.trigon s (i x1) (i y1) (i x2) (i y2) (i x3) (i y3) =<< colorToPixel c
aaTrigon x1 y1 x2 y2 x3 y3 c s = liftIO . SDL.aaTrigon s (i x1) (i y1) (i x2) (i y2) (i x3) (i y3) =<< colorToPixel c
filledTrigon x1 y1 x2 y2 x3 y3 c s = liftIO . SDL.filledTrigon s (i x1) (i y1) (i x2) (i y2) (i x3) (i y3) =<< colorToPixel c
polygon points c s = liftIO . SDL.polygon s (map (\(a, b) -> (i a, i b)) points) =<< colorToPixel c
aaPolygon points c s = liftIO . SDL.aaPolygon s (map (\(a, b) -> (i a, i b)) points) =<< colorToPixel c
filledPolygon points c s = liftIO . SDL.filledPolygon s (map (\(a, b) -> (i a, i b)) points) =<< colorToPixel c
texturedPolygon points texture dx dy s = liftIO $ SDL.texturedPolygon s (map (\(a, b) -> (i a, i b)) points) texture (i dx) (i dy)
bezier points steps c s = liftIO . SDL.bezier s (map (\(a, b) -> (i a, i b)) points) (i steps) =<< colorToPixel c

rotozoom src zoomx zoomy smooth = liftIO $ SDL.rotozoom src zoomx zoomy smooth
zoom src zoomx zoomy smooth = liftIO $ SDL.zoom src zoomx zoomy smooth
