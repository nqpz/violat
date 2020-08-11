-----------------------------------------------------------------------------
-- |
-- Module      :  BadRacing.Event
-- License     :  WTFPL 2.0
--
-- Stability   :  provisional
-- Portability :  portable
--
-- Events of the game.
--
-----------------------------------------------------------------------------
module BadRacing.Event where

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
import Data.Maybe
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
import BadRacing.Logic


parseEvent :: SDL.Event -> RacingState ()
parseEvent event = case event of
  SDL.KeyDown (Keysym k _ _) -> f addAction k
  SDL.KeyUp (Keysym k _ _) -> f' removeAction k
  SDL.Quit -> exit
  _ -> return ()
  where f g k = maybe (return ()) (either g runImmediate) $ parseKey k
        f' g k = maybe (return ()) (either g (const $ return ())) $ parseKey k

addAction :: LastingAction -> RacingState ()
addAction = modv' actions . S.insert

removeAction :: LastingAction -> RacingState ()
removeAction = modv' actions . S.delete

parseKey :: SDL.SDLKey -> Maybe (Either LastingAction ImmediateAction)
parseKey k = case k of
  SDLK_UP -> lasting GoingForward
  SDLK_DOWN -> lasting GoingBackward
  SDLK_RIGHT -> lasting GoingRight
  SDLK_LEFT -> lasting GoingLeft
  SDLK_SPACE -> immediate ToggleColorStop
  _ -> Nothing
  where lasting = Just . Left
        immediate = Just . Right
