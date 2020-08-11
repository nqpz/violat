-----------------------------------------------------------------------------
-- |
-- Module      :  BadRacing.Constants
-- License     :  WTFPL 2.0
--
-- Stability   :  provisional
-- Portability :  portable
--
-- Constants.
--
-----------------------------------------------------------------------------
module BadRacing.Constants where

viewDistance :: Double
viewDistance = 10000

createDistance :: Double
createDistance = 800

zDist :: Int -> Double
zDist height = 2 * fromIntegral height

z0Offset :: Double
z0Offset = 0.9
