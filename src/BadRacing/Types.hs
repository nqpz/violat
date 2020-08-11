{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  BadRacing.Types
-- License     :  WTFPL 2.0
--
-- Stability   :  provisional
-- Portability :  portable
--
-- Types.
--
-----------------------------------------------------------------------------
module BadRacing.Types where

import Control.Category
import Data.Label
import Prelude hiding ((.), id)
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Keysym as SDL
import qualified Data.Map as M
import qualified Data.Set as S

import Violat.Random
import Violat.Types (GameState, Game, Time, Color)

type Point2D = (Double, Double)
type Point3D = (Double, Double, Double)

data Object p = Polygon [p]
              | Simple p Double Point2D SimpleObject
              | Multi [Object p]
              deriving (Show, Eq, Ord)
type Object2D = Object Point2D
type Object3D = Object Point3D

data SimpleObject = Arc Double Double Double
                  | Rectangle Double Double
                  deriving (Show, Eq, Ord)

-- imperfect, a quadtree is needed for a better implementation
type ObjectSet = (M.Map Double Object3D,
                  M.Map Double Object3D)

data Axis = X | Y | Z

data LastingAction = GoingForward
                   | GoingBackward
                   | GoingLeft
                   | GoingRight
                   deriving (Show, Eq, Ord)

data ImmediateAction = ToggleColorStop
                     deriving (Show, Eq, Ord)

instance Show (Time -> Object3D -> Object3D) where
  show f = "<func>"

data Racing = Racing { _objects :: ObjectSet
                     , _position :: Point2D
                     , _direction :: Double
                     , _speed :: Double
                     , _actions :: S.Set LastingAction
                     , _animations :: M.Map Object3D (Time -> Object3D -> Object3D)
                     , _colors :: Maybe (Color, Color)
                     , _stopColor :: Bool
                     }
            deriving (Show)
mkLabel ''Racing

type RandomMonad = RandomMT IO
type RacingState r = GameState RandomMonad Racing r
type RacingGame = Game RandomMonad Racing
