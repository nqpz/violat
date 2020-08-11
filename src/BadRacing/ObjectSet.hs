-----------------------------------------------------------------------------
-- |
-- Module      :  BadRacing.ObjectSet
-- License     :  WTFPL 2.0
--
-- Stability   :  provisional
-- Portability :  portable
--
-- Racing game.
--
-----------------------------------------------------------------------------
module BadRacing.ObjectSet (
  emptyObjectSet,
  findNearbyObjects,
  addObject,
  removeObject,
  modifyObject,
  listToObjectSet,
  from3DTo2D,
  withPoints
  ) where

import qualified Data.Map as M
import Data.List

import Violat.Types
import BadRacing.Types


nub' = map head . group . sort

findNearbyObjects :: Double -> Point2D -> ObjectSet -> [Object3D]
findNearbyObjects k (x, z) (s0, s1) = nub' $ intersect objs0 objs1
  where objs0 = M.elems $ fst $ M.split (x + k) $ snd $ M.split (x - k) s0
        objs1 = M.elems $ fst $ M.split (z + k) $ snd $ M.split (z - k) s1

emptyObjectSet :: ObjectSet
emptyObjectSet = (M.empty, M.empty)

from3DTo2D :: Point3D -> Point2D
from3DTo2D (x, y, z) = (x, z)

getObjectPoints :: Object a -> [a]
getObjectPoints obj = case obj of
  Polygon ps -> ps
  Simple p _ _ _ -> [p]
  Multi objs -> concatMap getObjectPoints objs

withPoints :: (a -> a) -> Object a -> Object a
withPoints f obj = case obj of
  Polygon ps -> Polygon $ map f ps
  Simple p r o s -> Simple (f p) r o s
  Multi objs -> Multi $ map (withPoints f) objs

getObjectPoints2D :: Object3D -> [Point2D]
getObjectPoints2D = map from3DTo2D . getObjectPoints

addObject :: ObjectSet -> Object3D -> ObjectSet
addObject (s0, s1) obj = (foldl linkPoint s0 $ map fst ps,
                          foldl linkPoint s1 $ map snd ps)
  where linkPoint objset d = M.insert d obj objset
        ps = getObjectPoints2D obj

removeObject :: ObjectSet -> Object3D -> ObjectSet
removeObject (s0, s1) obj = (foldl unlink s0 $ map fst ps,
                             foldl unlink s1 $ map snd ps)
  where unlink objset p = M.delete p objset
        ps = getObjectPoints2D obj

modifyObject :: ObjectSet -> (Object3D -> Object3D) -> Object3D -> (ObjectSet, Object3D)
modifyObject objset f obj = (addObject (removeObject objset obj) $ f obj, f obj)

listToObjectSet :: [Object3D] -> ObjectSet
listToObjectSet = foldl addObject emptyObjectSet
