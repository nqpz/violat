-----------------------------------------------------------------------------
-- |
-- Module      :  BadRacing.Helpers
-- License     :  WTFPL 2.0
--
-- Stability   :  provisional
-- Portability :  portable
--
-- Helpers.
--
-----------------------------------------------------------------------------
module BadRacing.Helpers where

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
import Data.Function (on)

import System.IO.Unsafe

import Violat.Types as E
import Violat.Runner as V
import Violat.Helpers as V
import Violat.Random as V
import Violat.Color as V
import Violat.Graphics as V
import qualified Violat.SDL as V
import Violat.Cairo as V


import BadRacing.Types
import BadRacing.ObjectSet


getv' x = getv (x . gameSettings)
setv' x t = setv (x . gameSettings) t
modv' x f = modv (x . gameSettings) f

liftRand m = lift m

for = flip map

(.>) :: Category cat => cat a b -> cat b c -> cat a c
(.>) = flip (.)
infixr 9 .>

($>) = flip ($)
infixr 0 $>

prevsAndNexts :: [a] -> [(a, a, a)]
prevsAndNexts xs = map (\(a, (b, c)) -> (a, b, c))
                   $ zip (last xs : init xs)
                   $ zip xs (tail xs ++ [head xs])

unPrevsAndNexts :: [(a, a, a)] -> [a]
unPrevsAndNexts = map (\(_, p, _) -> p)

withNeighbors :: [a] -> [(a, a)]
withNeighbors xs = zip xs (tail xs ++ [head xs])

randOpaqueColor :: RacingState Color
randOpaqueColor = do
  r <- liftRand $ randomR 0 255
  g <- liftRand $ randomR 0 255
  b <- liftRand $ randomR 0 255
  return $ fromRGBAw r g b 255
