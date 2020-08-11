{-# LANGUAGE TupleSections #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  BadRacing.Math
-- License     :  WTFPL 2.0
--
-- Stability   :  provisional
-- Portability :  portable
--
-- Math-related functions.
--
-----------------------------------------------------------------------------
module BadRacing.Math where

import Prelude hiding ((.), id)
import Control.Category
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

import BadRacing.Types
import BadRacing.Helpers
import BadRacing.ObjectSet
import BadRacing.Constants


rotateObject3D :: Point3D -> Double -> Axis -> Object3D -> Object3D
rotateObject3D (xOrigo, yOrigo, zOrigo) angle axis = withPoints rotatePoint
  where rotatePoint (x, y, z) = case axis of
          X -> (xOrigo + x',
                yOrigo + y' * c - z' * s,
                zOrigo + y' * s + z' * c)
          Y -> (xOrigo + z' * s + x' * c,
                yOrigo + y',
                zOrigo + z' * c - x' * s)
          Z -> (xOrigo + x' * c - y' * s,
                yOrigo + x' * s + y' * c,
                zOrigo + z')
          where (x', y', z') = (x - xOrigo, y - yOrigo, z - zOrigo)
        (s, c) = (sin angle, cos angle)

type Plane = ()
flipObject3D :: Point3D -> Plane -> Object3D -> Object3D
flipObject3D (xOrigo, yOrigo, zOrigo) plane = withPoints flipPoint
  where flipPoint (x, y, z) = undefined -- very mathy, but useful if implemented

flipObject3DOnYOrigo :: Double -> Object3D -> Object3D
flipObject3DOnYOrigo yOrigo = withPoints flipPoint
  where flipPoint (x, y, z) = (x, 2 * yOrigo - y, z)

rotatePoint2D :: Point2D -> Double -> Point2D -> Point2D
rotatePoint2D (xOrigo, zOrigo) angle (x, z)
  = (xOrigo + x' * cos angle - z' * sin angle,
     zOrigo + z' * cos angle + x' * sin angle)
  where (x', z') = (x - xOrigo, z - zOrigo)

rotatePoint3DAs2D :: Point2D -> Double -> Point3D -> Point3D
rotatePoint3DAs2D origo angle (x, y, z) =
  uncurry (, y,) $ rotatePoint2D origo angle (x, z)

transformToOrigo :: Point2D -> Double -> Double -> Object3D -> Object3D
transformToOrigo origo@(xOrigo, zOrigo) dir zPlayerInside = withPoints transform
  where transform p0 = p2
          where (x1, y1, z1) = rotatePoint3DAs2D origo dir p0
                p2 = (x1 - xOrigo, y1, z1 - zOrigo + zPlayerInside)

clipAt0 :: (Int, Int) -> Object3D -> Object3D
clipAt0 res obj = case obj of
  Polygon ps -> Polygon $ clipPolyAt0 res ps
  s@(Simple _ _ _ _) -> s
  Multi objs -> Multi $ map (clipAt0 res) objs

clipPolyAt0 :: (Int, Int) -> [Point3D] -> [Point3D]
clipPolyAt0 (_, h) ps | all (\(_, _, z) -> z < dist) ps = []
                      | otherwise = simplify ps $> clipPoints
  where simplify :: [Point3D] -> [Point3D]
        simplify = prevsAndNexts
                   .> filter isRelevant
                   .> unPrevsAndNexts
        isRelevant ((_, _, prevZ), (_, _, curZ), (_, _, nextZ))
          = prevZ >= dist || nextZ >= dist || curZ >= dist

        clipPoints :: [Point3D] -> [Point3D]
        clipPoints = concatMap clipPoint . prevsAndNexts

        clipPoint :: (Point3D, Point3D, Point3D) -> [Point3D]
        clipPoint (prev@(prevX, prevY, prevZ),
                   cur@(curX, curY, curZ),
                   next@(nextX, nextY, nextZ))
          | curZ < dist = poss
          | otherwise = [cur]
          where poss | prevZ < dist = [clip1 next]
                     | nextZ < dist = [clip1 prev]
                     | otherwise = [clip1 prev, clip1 next]
                clip1 :: Point3D -> Point3D
                clip1 (x0, y0, z0) = (xFinal, yFinal, dist)
                  where (x1, y1, z1) = cur
                        xFinal = x0 + (z0 - dist) * (x1 - x0) / (z0 - z1)
                        yFinal = y0 + (z0 - dist) * (y1 - y0) / (z0 - z1)
        dist = -(zDist h * z0Offset)

transformTo2D :: (Int, Int) -> Object3D -> Object2D
transformTo2D res@(w, h) obj = case obj of
  Polygon ps -> Polygon $ map transformPoint ps
  Simple p@(x, y, z) resize offset shape ->
    Simple (transformPoint p) (resize * nScale z) offset shape
  Multi objs -> Multi $ map (transformTo2D res) objs
  where transformPoint (x, y, z) = (x * nScale z, (y - fromIntegral h) * nScale z + fromIntegral h)
        nScale z = dist / (z + dist)
          where dist = zDist h

transformToScreen :: (Int, Int) -> Object2D -> Object2D
transformToScreen (w, h) = withPoints transform
  where transform (x, y) = (x + fromIntegral w / 2, fromIntegral h - y)
