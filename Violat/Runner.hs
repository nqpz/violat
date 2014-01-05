{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Violat.Runner
-- License     :  WTFPL 2.0
--
-- Stability   :  provisional
-- Portability :  portable
--
-- Game engine.
--
-----------------------------------------------------------------------------
module Violat.Runner where

#include "violat.h"

import Prelude hiding ((.), id)
import Control.Category
import qualified Data.Label as L
import Control.Concurrent (threadDelay)
import Control.Monad.State
import qualified Graphics.UI.SDL as SDL
import Data.Ratio
import Control.Monad.Maybe

import Violat.Types
import Violat.Helpers


emptyGame :: BASE => a -> Game m a
emptyGame a = Game { _gameSettings = a
                   , _gameStartTime = 0
                   , _gameFPSTarget = Nothing
                   , _currentFPS = 0
                   , _timeAction = Nothing
                   , _newTimeStart = 0
                   , _prevDelay = 0
                   , _frameNumber = 0
                   , _eventAction = Nothing
                   , _stepAction = Nothing
                   , _windowResolution = (0, 0)
                   , _windowTitle = ""
                   , _screenSurf = Nothing
                   , _keepRunning = True
                   }

-- | Runs
runGame :: BASE => GameState m a r -> Game m a -> m (Game m a)
runGame a b = execStateT a b

modifySettings :: BASE => (a -> a) -> GameState m a ()
modifySettings = modv gameSettings

exit :: BASE => GameState m a ()
exit = setv keepRunning False

stepAct :: BASE => StepActor m a
stepAct tDiff = getv stepAction >>>= ($ tDiff)

timeAct :: BASE => TimeActor m a
timeAct t0 t1 = getv timeAction >>>= (\f -> f t0 t1)

eventAct :: BASE => EventActor m a
eventAct ev = getv eventAction >>>= ($ ev)
  
playGame :: BASE => GameState m a ()
playGame = do
  (width, height) <- getv windowResolution
  title <- getv windowTitle
  liftIO $ do
    SDL.init [SDL.InitEverything]
    SDL.setVideoMode width height 32 [SDL.DoubleBuf]
    SDL.setCaption title title
  setv screenSurf =<< liftM Just (liftIO SDL.getVideoSurface)
  setv gameStartTime =<< liftIO getTicks
  gameLoop
  liftIO SDL.quit

takeWhileM :: Monad m => (a -> Bool) -> [m a] -> m [a]
takeWhileM cond (x : xs) = do
  y <- x
  if cond y
    then do
    rest <- takeWhileM cond xs
    return (y : rest)
    else do
    return []

pollEvents :: IO [SDL.Event]
pollEvents = takeWhileM (/= SDL.NoEvent) $ repeat SDL.pollEvent

gameLoop :: BASE => GameState m a ()
gameLoop = do
  events <- liftIO pollEvents
  mapM_ eventAct events
  
  startTime <- getv gameStartTime
  t1 <- liftM (\n -> n - startTime) $ liftIO getTicks
  t0 <- getv newTimeStart
  setv newTimeStart t1
  let tDiff = (t1 - t0)
  setv currentFPS (1000 % (if tDiff == 0 then 1 else tDiff))

  stepAct tDiff

  timeAct t0 t1

  still <- getv keepRunning
  if still then do
    liftIO . SDL.flip =<<< getv screenSurf

    prevD <- getv prevDelay
    calculateDelay (tDiff - prevD) >>>= \d -> do
      liftIO $ threadDelay (d * 1000)
      setv prevDelay d

    modv frameNumber (+1)
    gameLoop

    else return ()

calculateDelay :: BASE => Time -> GameState m a (Maybe Int)
calculateDelay diff = getv gameFPSTarget >><= \fps -> do
  return $ clamp $ round (1000 / fps - fromIntegral diff)
