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
import Control.Monad.Trans.Maybe
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
lineToPoint = uncurry Cairo.lineTo

relLineToPoint :: Point -> Cairo.Render ()
relLineToPoint = uncurry Cairo.relLineTo

moveToPoint :: Point -> Cairo.Render ()
moveToPoint = uncurry Cairo.moveTo

relMoveToPoint :: Point -> Cairo.Render ()
relMoveToPoint = uncurry Cairo.relMoveTo

translatePoint :: Point -> Cairo.Render ()
translatePoint = uncurry Cairo.translate
