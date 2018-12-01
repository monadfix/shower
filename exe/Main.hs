module Main where

import Shower (showerString)
import Control.Exception (evaluate)
import System.Exit (die)

main :: IO ()
main = do
  s <- getContents
  _ <- evaluate (length s) -- strict 'getContents'
  either die putStrLn $ showerString s
