{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Violat.Cairo
-- License     :  WTFPL 2.0
--
-- Stability   :  provisional
-- Portability :  portable
--
-- Cairo additions.
--
-----------------------------------------------------------------------------
module Violat.Cairo where

#include "violat.h"

import Prelude hiding ((.), id)
import Control.Category
import qualified Data.Label as L
import Control.Monad.State
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.Rendering.Cairo as Cairo
import Data.Ratio
import Control.Monad.Maybe
import Data.Maybe
import Foreign.Ptr (castPtr)

import Violat.Types
import Violat.Helpers
import Violat.Graphics
import Violat.Color


cairo :: BASE => Cairo.Render () -> SDL.Surface -> Int -> Int -> GameState m a ()
cairo drawing surf w h = do
  pixels <- liftIO $ liftM castPtr $ SDL.surfaceGetPixels surf
  liftIO $ Cairo.withImageSurfaceForData pixels Cairo.FormatARGB32 w h (w * 4) $ \canvas ->
    Cairo.renderWith canvas drawing

cairoOnScreen :: BASE => Cairo.Render () -> GameState m a ()
cairoOnScreen drawing = (>> return ()) $ runMaybeT $ do
  surf <- MaybeT $ getv screenSurf
  (w, h) <- justT $ getv windowResolution
  justT $ cairo drawing surf w h

setSourceColor :: Color -> Cairo.Render ()
setSourceColor (RGBA r g b a) = Cairo.setSourceRGBA r g b a

lineToPoint :: Point -> Cairo.Render ()
lineToPoint (x, y) = Cairo.lineTo x y

relLineToPoint :: Point -> Cairo.Render ()
relLineToPoint (x, y) = Cairo.relLineTo x y

moveToPoint :: Point -> Cairo.Render ()
moveToPoint (x, y) = Cairo.moveTo x y

relMoveToPoint :: Point -> Cairo.Render ()
relMoveToPoint (x, y) = Cairo.relMoveTo x y
