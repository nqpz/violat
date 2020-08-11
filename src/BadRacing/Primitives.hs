-----------------------------------------------------------------------------
-- |
-- Module      :  BadRacing.Primitives
-- License     :  WTFPL 2.0
--
-- Stability   :  provisional
-- Portability :  portable
--
-- 3D primitives.
--
-----------------------------------------------------------------------------
module BadRacing.Primitives where

import BadRacing.Types
import BadRacing.Helpers
import BadRacing.Math


polygon :: [Point3D] -> Object3D
polygon = Polygon

triangle :: Point3D -> Point3D -> Point3D -> Object3D
triangle a b c = polygon [a, b, c]

arc :: Point3D -> Point2D -> Double -> Double -> Double -> Object3D
arc p offset radius angle1 angle2 = Simple p 1 offset $ Arc radius angle1 angle2

rectangle :: Point3D -> Point2D -> Double -> Double -> Object3D
rectangle p offset width height = Simple p 1 offset $ Rectangle width height

nGon :: Point3D -> Int -> Double -> Object3D
nGon (x, y, z) nSides radius = Polygon $ map getPoint angles
  where angle = 2 * pi / fromIntegral nSides
        angles = take nSides [0, angle..]
        getPoint a = (x + sin a * radius, y, z + cos a * radius)

pyramid :: Point3D -> Int -> Double -> Double -> Object3D
pyramid p@(x, y, z) nSides radius height = Multi (baseObj : sides)
  where baseObj@(Polygon base) = nGon p nSides radius
        top = (x, y + height, z)
        sides = map (uncurry makeSide) $ withNeighbors (last base : base)
        makeSide a b = polygon [a, b, top]

diamond :: Point3D -> Int -> Double -> Double -> Object3D
diamond p@(_, y, _) nSides radius height = Multi [pyr, pyr']
  where pyr = pyramid p nSides radius (height / 2)
        pyr' = flipObject3DOnYOrigo y pyr

cylinder :: Point3D -> Int -> Double -> Double -> Object3D
cylinder p@(x, y, z) nSides radius height = Multi (bottomObj : topObj : sides)
  where bottomObj@(Polygon base) = nGon p nSides radius
        topObj = nGon (x, y + height, z) nSides radius
        sides = map (uncurry makeSide) $ withNeighbors (last base : base)
        makeSide (x0, y0, z0) (x1, y1, z1) = polygon [ (x0, y0, z0)
                                                     , (x1, y1, z1)
                                                     , (x1, y1 + height, z1)
                                                     , (x0, y0 + height, z0)
                                                     ]

rectangle3D :: Point3D -> Double -> Double -> Double -> Object3D
rectangle3D p@(x, y, z) wX wY wZ = Multi polygons
  where p0 = (x - wX / 2, y + wY / 2, z - wZ / 2)
        p1 = (x - wX / 2, y + wY / 2, z + wZ / 2)
        p2 = (x + wX / 2, y + wY / 2, z + wZ / 2)
        p3 = (x + wX / 2, y + wY / 2, z - wZ / 2)
        p4 = (x - wX / 2, y - wY / 2, z - wZ / 2)
        p5 = (x - wX / 2, y - wY / 2, z + wZ / 2)
        p6 = (x + wX / 2, y - wY / 2, z + wZ / 2)
        p7 = (x + wX / 2, y - wY / 2, z - wZ / 2)
        polygons = map polygon [ [p0, p1, p2, p3]
                               , [p4, p5, p6, p7]
                               , [p0, p1, p5, p4]
                               , [p1, p2, p6, p5]
                               , [p2, p3, p7, p6]
                               , [p3, p0, p4, p7]
                               ]

cube :: Point3D -> Double -> Object3D
cube p w = rectangle3D p w w w
