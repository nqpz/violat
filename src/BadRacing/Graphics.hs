-----------------------------------------------------------------------------
-- |
-- Module      :  BadRacing.Graphics
-- License     :  WTFPL 2.0
--
-- Stability   :  provisional
-- Portability :  portable
--
-- Graphics.
--
-----------------------------------------------------------------------------
module BadRacing.Graphics where

import Prelude hiding ((.), id)
import Control.Category
import qualified Data.Label as L
import Control.Monad
import Control.Monad.State
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Keysym as SDL
import Graphics.Rendering.Cairo as Cairo
import Data.Ratio
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Function (on)

import Violat.Types as V
import Violat.Runner as V
import Violat.Helpers as V
import Violat.Random as V
import Violat.Color as V
import Violat.Graphics as V
import qualified Violat.SDL as V
import Violat.Cairo as V

import BadRacing.Types
import BadRacing.Helpers
import BadRacing.ObjectSet
import BadRacing.Event
import BadRacing.Math
import BadRacing.Constants
import BadRacing.Logic

updateColors :: RacingState ()
updateColors = do
  c0 <- randOpaqueColor
  c1 <- randOpaqueColor
  setv' colors $ Just (c0, c1)

drawRacing :: Time -> RacingState ()
drawRacing tdiff = do
  pos@(xPos, zPos) <- getv' position
  dir <- getv' direction
  res@(w, h) <- getv windowResolution

  mustCreate <- liftM (null . findNearbyObjects createDistance pos) $ getv' objects
  when mustCreate $ do
    x <- liftRand $ randomR (-0.9) 0.9
    z <- liftRand $ randomR (-0.9) 0.9
    y <- liftRand $ randomR 0 600
    let p = (x * createDistance + xPos, y, z * createDistance + zPos)
    obj <- randObject p
    ani <- randAnimation p
    modv' objects (flip addObject obj)
    modv' animations (M.insert obj ani)

  objs <- liftM (findNearbyObjects viewDistance pos) $ getv' objects
  runAnimations tdiff objs
  objects <- liftM (findNearbyObjects viewDistance pos) $ getv' objects

  stop <- getv' stopColor
  when (not stop) updateColors
  Just (color0, color1) <- getv' colors

  -- clear screen
  onScreen $ V.fillRect Nothing color0

  let dist = zDist h
      objects' = for objects
                 $ transformToOrigo pos dir zPlayerInside
                 .> clipAt0 res
                 .> transformTo2D res
                 .> transformToScreen res

      zPlayerInside = dist * fromIntegral h
                      / (fromIntegral h - fromIntegral h / 2) - dist
      playerObject = transformToScreen res
                     $ transformTo2D res
                     $ Simple (0, 0, zPlayerInside) 1 (0, 0) $ Arc 30 pi (2 * pi)
      allObjects = playerObject : objects'
  mapM_ (drawObject color1) allObjects

drawObject color (Multi objs) = mapM_ (drawObject color) objs
drawObject color obj = do
  cairoOnScreen $ do
    setAntialias AntialiasNone
    setSourceColor color
    case obj of
      Polygon ps -> drawPoints ps
      Simple p r o s -> drawShape p r o s
    fill

drawPoints ps = case ps of
  p : ps -> do
    moveToPoint p
    mapM_ lineToPoint ps
  [] -> return ()

drawShape p resize offset shape = do
  translatePoint p
  translatePoint offset
  scale resize resize
  case shape of
    Arc radius angle1 angle2 -> arc 0 0 radius angle1 angle2
    Rectangle w h -> rectangle (-w') (-h') w' h'
      where (w', h') = (w / 2, h / 2)
