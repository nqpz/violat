module Main where

import System.Environment (getArgs)
import BadRacing.Main


main :: IO ()
main = getArgs >>= runMain
