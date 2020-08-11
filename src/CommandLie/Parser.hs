{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  CommandLie.Parser
-- License     :  WTFPL 2.0
--
-- Stability   :  provisional
-- Portability :  portable
--
-- A simple command line parser.
--
-----------------------------------------------------------------------------
module CommandLie.Parser (
  Config,
  Setting(..),
  Results,
  parseCommandLine,
  formatOptions,
  getRest
  ) where

import qualified Data.Map as M
import Data.List
import Data.Maybe
import Safe
import System.IO.Unsafe

type FList a = [a] -> [a]

(++.) :: FList a -> [a] -> FList a
a ++. b = (++ b) . a

instance Show a => Show (FList a) where
  show = show . ($ [])


type Config = [Setting]
data Setting = Setting { sName :: String
                       , sShort :: String
                       , sLong :: String
                       , sNArgs :: Int
                       , sHelp :: String
                       , sDefault :: Maybe [String]
                       }
type Results = M.Map String [String]

data TempResults = TempResults { resultsMap :: M.Map String (FList String)
                               , tempFlagArgsRemaining :: Int
                               , tempFlagName :: Maybe String
                               , ignoresFlags :: Bool
                               }
                 deriving (Show)

adjustWithDefault :: Ord k => a -> (a -> a) -> k -> M.Map k a -> M.Map k a
adjustWithDefault d f k m = M.adjust f k m'
  where m' | not (k `M.member` m) = M.insert k d m
           | otherwise = m

parseCommandLine :: Config -> [String] -> Results
parseCommandLine config args = handle $ tempFlagName results
  where handle (Just name) = M.delete name rmap
        handle _ = rmap

        rmap = M.map ($ []) $ resultsMap results
        results = foldl' getResults
                  (TempResults { resultsMap = makeDefaults
                               , tempFlagArgsRemaining = 0
                               , tempFlagName = Nothing
                               , ignoresFlags = False
                               }) args

        makeDefaults = M.fromList $ catMaybes $ map mkd config
          where mkd (Setting name _ _ _ _ (Just d)) = Just (name, const d)
                mkd _ = Nothing

        getResults r arg
                  | ignoresFlags r = r { resultsMap = adjustWithDefault id (++. [arg]) "_rest" $ resultsMap r
                                       }

                  | isJust (tempFlagName r) && tempFlagArgsRemaining r > 0 =
                    r { resultsMap = adjustWithDefault id (++. [arg]) (fromJust $ tempFlagName r)
                                     $ resultsMap r
                      , tempFlagArgsRemaining = tempFlagArgsRemaining r - 1
                      , tempFlagName = if tempFlagArgsRemaining r - 1 == 0
                                       then Nothing else tempFlagName r
                      }

                  | arg == "--" = r { ignoresFlags = True
                                    }

                  | isPrefixOf "--" arg && isJust longLookedUp =
                      r { tempFlagName = if longLkN > 0 then Just longLkName else Nothing
                        , tempFlagArgsRemaining = longLkN
                        , resultsMap = M.insert longLkName id $ resultsMap r
                        }

                  | isPrefixOf "-" arg && not (isPrefixOf "-" (tail arg)) =
                        foldl' (\r (name, n) -> r { tempFlagName = if n > 0 then Just name else Nothing
                                                  , tempFlagArgsRemaining = n
                                                  , resultsMap = M.insert name id $ resultsMap r
                                                  }
                               ) r shortLookedUp

                  | otherwise = r { resultsMap = adjustWithDefault id (++. [arg]) "_rest" $ resultsMap r
                                  }
          where longLookedUp = M.lookup (tailSafe $ tailSafe arg) longMap
                (longLkName, longLkN) = (fst $ fromJust longLookedUp,
                                         snd $ fromJust longLookedUp)
                shortLookedUp = catMaybes $ map (\c -> M.lookup c shortMap)
                                $ tailSafe arg
        (shortMap, longMap) = foldl' sub (M.empty, M.empty) config
          where sub (ms, ml) (Setting name short long n _ _)
                  = (foldl' (\m s -> M.insert s (name, n) m) ms short,
                     M.insert long (name, n) ml)



formatOptions :: Config -> String
formatOptions = intercalate "\n" . map showSetting
  where showSetting :: Setting -> String
        showSetting (Setting _ short long _ help defaultVal)
          = "  " ++ flags ++ "    " ++ help ++ " (default: " ++ handleDefault defaultVal ++ ")"
          where flags = intercalate ", " $
                        map (\c -> ['-', c]) short ++ ["--" ++ long]
                handleDefault Nothing = "not in effect"
                handleDefault (Just val) = intercalate " " val

getRest :: Results -> [String]
getRest =  (M.! "_rest")
