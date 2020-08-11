-----------------------------------------------------------------------------
-- |
-- Module      :  BadRacing.Main
-- License     :  WTFPL 2.0
--
-- Stability   :  provisional
-- Portability :  portable
--
-- Racing game.
--
-----------------------------------------------------------------------------
module BadRacing.Main where

import BadRacing.Runner
import CommandLie.Parser

import qualified Data.Map as M

config :: Config
config = [ Setting "showHelp" "h" "help" 0 "show this help message and exit" Nothing
         , Setting "showVersion" "V" "version" 0 "show version information and exit" Nothing
         , Setting "windowWidth" "w" "window-width" 1 "the width of the window" (Just ["800"])
         , Setting "windowHeight" "h" "window-height" 1 "the height of the window" (Just ["600"])
         , Setting "debug" "d" "debug" 0 "print debug messages" Nothing
         ]

showHelp = do
  putStrLn "Usage: badracing [OPTION] FILE..."
  putStrLn "A poor 3D game"
  putStrLn $ formatOptions config

showVersion = putStrLn "\
\badracing 0.1.0\n\
\Copyright (C) 2013 Niels G. W. Serup\n\
\This is free software under the terms of the Do What The Fuck You Want To Public\n\
\License (WTFPL); see <http://wtfpl.net/>."

runMain :: [String] -> IO ()
runMain args
  | "showHelp" `M.member` settings = showHelp
  | "showVersion" `M.member` settings = showVersion
  | otherwise = (>> return ()) $ runRacing
                (read $ head (settings M.! "windowWidth"))
                (read $ head (settings M.! "windowHeight"))
                ("debug" `M.member` settings)
  where settings :: Results
        settings = parseCommandLine config args
