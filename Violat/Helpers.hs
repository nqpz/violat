{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Violat.Helpers
-- License     :  WTFPL 2.0
--
-- Stability   :  provisional
-- Portability :  portable
--
-- Game engine.
--
-----------------------------------------------------------------------------
module Violat.Helpers where

#include "violat.h"

import Prelude hiding ((.), id)
import Control.Category
import qualified Data.Label as L
import Control.Concurrent (threadDelay)
import Control.Monad.State
import qualified Graphics.UI.SDL as SDL
import Data.Ratio
import Control.Monad.Maybe
import Data.Maybe

import Violat.Types

getTicks :: IO Int
getTicks = liftM fromIntegral SDL.getTicks

getv :: BASE => (Game m a L.:-> b) -> GameState m a b
getv x = return . L.get x =<< get

setv :: BASE => (Game m a L.:-> b) -> b -> GameState m a ()
setv x t = modify $ L.set x t

modv :: BASE => (Game m a L.:-> b) -> (b -> b) -> GameState m a ()
modv x f = modify $ L.modify x f

(>>>=) :: Monad m => m (Maybe t) -> (t -> m ()) -> m ()
x >>>= f = x >>= g
  where g (Just y) = f y
        g Nothing = return ()
infixl 1 >>>=

f =<<< x = x >>>= f
infixr 1 =<<<

x >><= f = x >>= g
  where g (Just y) = return . Just =<< f y
        g Nothing = return Nothing
infixl 1 >><=

f =><< x = x >><= f
infixr 1 =><<

justT :: BASE => m a -> MaybeT m a
justT = MaybeT . liftM Just

runJustT :: BASE => MaybeT m a -> m a
runJustT = liftM fromJust . runMaybeT

clamp :: (Num n, Ord n) => n -> n
clamp n | n < 0 = 0
        | otherwise = n

timeActorSimple :: BASE => Int -> (Int -> Action m a) -> TimeActor m a
timeActorSimple often action t0 t1 = mapM_ action times
  where times = filter (\t -> t `rem` often == 0) [t0 + 1..t1]

multiTimeActors :: BASE => [TimeActor m a] -> TimeActor m a
multiTimeActors actors t0 t1 = mapM_ (\a -> a t0 t1) actors

multiStepActors :: BASE => [StepActor m a] -> StepActor m a
multiStepActors actors tDiff = mapM_ ($ tDiff) actors

formatRatio :: (Integral a, Show a) => Int -> Ratio a -> String
formatRatio n r = show a ++ rest
  where (a : ds) = ratioExpansions' n r
        rest | null ds = ""
             | otherwise = "." ++ concatMap show ds

ratioExpansions' :: Integral a => Int -> Ratio a -> [a]
ratioExpansions' nDigits r | length digits == nDigits + 2 = roundIt digits
                           | otherwise = digits
  where digits = take (nDigits + 2) es'
        es = ratioExpansions r
        es' | null es = [0]
            | otherwise = es

roundIt ds | x1 >= 5 = roundIt' ds' x0
           | otherwise = ds' ++ [x0]
  where ds' = take (length ds - 2) ds
        x0 = last $ init ds
        x1 = last ds
        roundIt' [] s0 = [s0 + 1]
        roundIt' ss s0 | s0 < 9 = ss ++ [s0 + 1]
                       | otherwise = roundIt' (init ss) (last ss)

ratioExpansions :: Integral a => Ratio a -> [a]
ratioExpansions 0 = []
ratioExpansions r = n `div` d : ratioExpansions (((n `mod` d) * 10) % d)
  where (n, d) = (numerator r, denominator r)
