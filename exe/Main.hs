module Main where

import Shower (showerString)
import Control.Exception (evaluate)

main :: IO ()
main = do
  s <- getContents
  _ <- evaluate (length s) -- strict 'getContents'
  putStrLn (showerString s)
