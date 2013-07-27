{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Violat.Random
-- License     :  WTFPL 2.0
--
-- Stability   :  provisional
-- Portability :  portable
--
-- Useful random functions for the Rand monad.
--
-----------------------------------------------------------------------------
module Violat.Random where

import Control.Monad
import qualified Control.Monad.Random as CMR
import qualified Data.Random as DR
import qualified Data.Random.Extras as DRE
import Data.Random.Distribution.Uniform
import Data.Word (Word64)

type RandomState = CMR.Rand CMR.StdGen
type RandomStateT = CMR.RandT CMR.StdGen

-- | Makes a function that returns a DR.RVar usable by e.g. CMR.Rand.
randomSt :: forall m a . CMR.MonadRandom m
            => DR.RVar a -> m a
randomSt rvar = DR.runRVar rvar (CMR.getRandom :: m Word64)

-- | Random number in range @a..b@.
randomR :: (CMR.Random a, CMR.MonadRandom m) => a -> a -> m a
randomR a b = CMR.getRandomR (a, b)

-- | Random element from list.
choice :: CMR.MonadRandom m
          => [a] -> m a
choice = randomSt . DRE.choice

-- | Nothing if empty list.
choiceMay :: CMR.MonadRandom m
          => [a] -> Maybe (m a)
choiceMay = liftM randomSt . DRE.safeChoice

-- | Random element from weighted list.
weightedChoice :: CMR.MonadRandom m => [(a, Rational)] -> m a
weightedChoice = CMR.fromList

weightedChoiceMay :: CMR.MonadRandom m => [(a, Rational)] -> Maybe (m a)
weightedChoiceMay [] = Nothing
weightedChoiceMay xs = Just (CMR.fromList xs)

-- | Random sample from list.
sample :: CMR.MonadRandom m
          => Int -> [a] -> m [a]
sample n = randomSt . DRE.sample n

-- | Shuffles list.
shuffle :: CMR.MonadRandom m
           => [a] -> m [a]
shuffle = randomSt . DRE.shuffle

-- | Runs in IO.
runRandTIO :: Monad m => RandomStateT m a -> IO (m (a, CMR.StdGen))
runRandTIO x = liftM (runRandT x) CMR.newStdGen

-- | Evaluates in IO.
evalRandTIO :: Monad m => RandomStateT m a -> IO (m a)
evalRandTIO x = liftM (evalRandT x) CMR.newStdGen

runRandT :: (Monad m, CMR.RandomGen g) => CMR.RandT g m a -> g -> m (a, g)
runRandT = CMR.runRandT

evalRandT :: (Monad m, CMR.RandomGen g) => CMR.RandT g m a -> g -> m a
evalRandT = CMR.evalRandT

evalRand :: CMR.RandomGen g => CMR.Rand g a -> g -> a
evalRand = CMR.evalRand

runRand :: CMR.RandomGen g => CMR.Rand g a -> g -> (a, g)
runRand = CMR.runRand

evalRandIO :: CMR.Rand CMR.StdGen a -> IO a
evalRandIO = CMR.evalRandIO
