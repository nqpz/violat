{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Violat.Types
-- License     :  WTFPL 2.0
--
-- Stability   :  provisional
-- Portability :  portable
--
-- Game engine.
--
-----------------------------------------------------------------------------
module Violat.Types where

#include "violat.h"

import Prelude hiding ((.), id)
import Control.Category
import qualified Data.Label as L
import Control.Monad
import Control.Monad.State
import Data.Ratio
import Data.Word
import qualified Graphics.UI.SDL as SDL


type Action m a = GameState m a ()
type TimeActor m a = Time -> Time -> Action m a
type EventActor m a = SDL.Event -> Action m a
type StepActor m a = Time -> Action m a
type DrawActor m a = Action m a
type Time = Int -- milliseconds

type GameState m a r = StateT (Game m a) m r

data Game m a = Game { _gameSettings :: a
                     , _gameStartTime :: Time
                     , _gameFPSTarget :: Maybe (Ratio Time)
                     , _currentFPS :: Ratio Time
                     , _timeAction :: Maybe (TimeActor m a)
                     , _newTimeStart :: Time
                     , _prevDelay :: Time
                     , _frameNumber :: Int
                     , _eventAction :: Maybe (EventActor m a)
                     , _stepAction :: Maybe (StepActor m a)
                     , _drawAction :: Maybe (DrawActor m a)
                     , _windowResolution :: (Int, Int)
                     , _windowTitle :: String
                     , _screenSurf :: Maybe SDL.Surface
                     , _keepRunning :: Bool
                     }
L.mkLabel ''Game

data Color = RGBA { r :: Double
                  , g :: Double
                  , b :: Double
                  , a :: Double
                  }
           deriving (Show, Read, Eq, Ord)

type Point = (Double, Double)
