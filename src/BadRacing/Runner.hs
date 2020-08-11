-----------------------------------------------------------------------------
-- |
-- Module      :  BadRacing.Runner
-- License     :  WTFPL 2.0
--
-- Stability   :  provisional
-- Portability :  portable
--
-- Racing game.
--
-----------------------------------------------------------------------------
module BadRacing.Runner where

import Prelude hiding ((.), id)
import Control.Category
import qualified Data.Label as L
import Control.Monad
import Control.Monad.State
import Data.Ratio
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Function (on)

import Violat.Types as V
import Violat.Runner as V
import Violat.Helpers as V
import Violat.Random as V
import Violat.Color as V
import Violat.Graphics as V
import qualified Violat.SDL as V
import Violat.Cairo as V

import BadRacing.Types
import BadRacing.Helpers
import BadRacing.ObjectSet
import BadRacing.Event
import BadRacing.Math
import BadRacing.Graphics
import BadRacing.Logic
import BadRacing.Primitives


runRacing :: Int -> Int -> Bool -> IO RacingGame
runRacing w h debug = join
                      $ evalRandTIO
                      $ V.runGame (startRacing w h debug >> playGame >> endRacing)
                      $ emptyGame emptyRacing

emptyRacing :: Racing
emptyRacing = Racing { _objects = emptyObjectSet
                     , _position = (0, 0)
                     , _direction = 0
                     , _speed = 0
                     , _actions = S.empty
                     , _animations = M.empty
                     , _colors = Nothing
                     , _stopColor = False
                     }

startRacing :: Int -> Int -> Bool -> RacingState ()
startRacing w h debug = do
  setv windowTitle "Bad Racing"
  setv windowResolution (w, h)
  setv gameFPSTarget $ Just 60
  setv eventAction $ Just parseEvent
  setv stepAction $ Just $ multiStepActors [ runLasting
                                           , updatePosition
                                           , drawRacing
                                           ]
  when debug $ modv stepAction $ fmap (>> const printStats)
  updateColors
  liftIO $ putStrLn "Starting game."

endRacing :: RacingState ()
endRacing = do
  liftIO $ putStrLn "Ending game."

printStats :: RacingState ()
printStats = do
  pos <- liftM show $ getv' position
  dir <- liftM show $ getv' direction
  spd <- liftM show $ getv' speed
  fps <- liftM (formatRatio 2) $ getv currentFPS
  liftIO $ putStrLn ("pos: " ++ pos ++
                     ", dir: " ++ dir ++
                     ", speed: " ++ spd ++
                     ", fps:" ++ fps)
