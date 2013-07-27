{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Violat.Graphics
-- License     :  WTFPL 2.0
--
-- Stability   :  provisional
-- Portability :  portable
--
-- Miscellaneous graphics additions and fixes.
--
-----------------------------------------------------------------------------
module Violat.Graphics where

#include "violat.h"

import Prelude hiding ((.), id)
import Control.Category
import qualified Data.Label as L
import Control.Monad.State
import qualified Graphics.UI.SDL as SDL
import Data.Ratio
import Control.Monad.Maybe
import Data.Maybe

import Violat.Types
import Violat.Helpers
import Violat.Color


colorToPixel :: BASE => Color -> GameState m a SDL.Pixel
colorToPixel color = runJustT $ do
  surf <- MaybeT $ getv screenSurf
  liftIO $ getPixel $ SDL.surfaceGetPixelFormat surf
  where getPixel format = case color of
          RGBA r g b a -> SDL.mapRGBA format (w r) (w g) (w b) (w a)
        w = colorDoubleToWord8

onScreen1 :: BASE => (SDL.Surface -> GameState m a b) -> GameState m a b
onScreen1 f = runJustT $ do
  surf <- MaybeT $ getv screenSurf
  justT $ f surf

onScreen :: BASE => (SDL.Surface -> GameState m a b) -> GameState m a ()
onScreen = (>> return ()) . onScreen1
