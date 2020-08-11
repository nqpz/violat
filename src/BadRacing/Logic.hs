-----------------------------------------------------------------------------
-- |
-- Module      :  BadRacing.Logic
-- License     :  WTFPL 2.0
--
-- Stability   :  provisional
-- Portability :  portable
--
-- Logic.
--
-----------------------------------------------------------------------------
module BadRacing.Logic where

import Prelude hiding ((.), id)
import Control.Category
import qualified Data.Label as L
import Control.Monad
import Control.Monad.State
import Data.Ratio
import Data.List
import Data.Maybe
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
import BadRacing.Math
import BadRacing.Primitives


updatePosition :: Time -> RacingState ()
updatePosition tdiff = do
  spd <- getv' speed
  dir <- getv' direction
  let dist = spd * fromIntegral tdiff / 1000.0
  let (xd, zd) = (sin dir * dist, cos dir * dist)
  modv' position (\(x, z) -> (x + xd, z + zd))

runImmediate :: ImmediateAction -> RacingState ()
runImmediate action = case action of
  ToggleColorStop -> modv' stopColor not

runLasting' :: Time -> S.Set LastingAction -> RacingState ()
runLasting' tdiff actions = do
  spd <- getv' speed
  let v0 | has GoingForward = (spd + k)
         | has GoingBackward = (spd - k)
         | otherwise = spd
  setv' speed v0
  spd <- getv' speed
  let v1 | spd > 0 && (not $ has GoingForward) = if spd - k < 0 then 0 else spd - k
         | spd < 0 && (not $ has GoingBackward) = if spd + k > 0 then 0 else spd + k
         | otherwise = spd
  setv' speed v1
  dir <- getv' direction
  let d | has GoingRight = (dir + q)
        | has GoingLeft = (dir - q)
        | otherwise = dir
  setv' direction d
  where k = 1000 * fromIntegral tdiff / 1000
        q = 0.9 * fromIntegral tdiff / 1000 * (if has GoingBackward && not (has GoingForward) then (-1) else 1)
        has = (`S.member` actions)

runLasting :: Time -> RacingState ()
runLasting tdiff = getv' actions >>= (runLasting' tdiff)

runAnimations :: Time -> [Object3D] -> RacingState ()
runAnimations tdiff objs = do
  anis <- getv' animations
  mapM_ (animate anis) objs
  where animate anis obj = do
          maybe (return ()) animate' $ M.lookup obj anis
          where animate' ani = do
                  objset <- getv' objects
                  let (objset', obj') = modifyObject objset (ani tdiff) obj
                  setv' objects objset'
                  modv' animations (M.delete obj)
                  modv' animations (M.insert obj' ani)


randAnimation :: Point3D -> RacingState (Time -> Object3D -> Object3D)
randAnimation p = do
  angle <- liftRand $ randomR 0.1 0.4
  axis <- liftRand $ choice [X, Y, Z]
  return $ \tdiff -> rotateObject3D p (angle * fromIntegral tdiff / 1000) axis

randObject :: Point3D -> RacingState Object3D
randObject p = join $ liftRand $ choice
               [ rNGon
               , rPyramid
               , rCylinder
               , rDiamond
               , rRectangle3D
               ]
  where rNGon = do
          nSides <- liftRand $ randomR 3 6
          radius <- liftRand $ randomR 50 300
          return $ nGon p nSides radius
        rPyramid = do
          nSides <- liftRand $ randomR 3 6
          radius <- liftRand $ randomR 50 300
          height <- liftRand $ randomR 50 500
          return $ pyramid p nSides radius height
        rDiamond = do
          nSides <- liftRand $ randomR 3 6
          radius <- liftRand $ randomR 50 300
          height <- liftRand $ randomR 50 500
          return $ diamond p nSides radius height
        rCylinder = do
          nSides <- liftRand $ randomR 3 6
          radius <- liftRand $ randomR 50 300
          height <- liftRand $ randomR 50 500
          return $ cylinder p nSides radius height
        rRectangle3D = do
          wX <- liftRand $ randomR 50 400
          wY <- liftRand $ randomR 50 400
          wZ <- liftRand $ randomR 50 400
          return $ rectangle3D p wX wY wZ
