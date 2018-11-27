module Shower.Class where

-- A tagless final encoding for a result builder (ShowS, Doc, Html, etc)
class Shower a where
  -- { x = 24, y = 42 }
  showerRecord :: [(a, a)] -> a
  -- [1, 2, 3]
  showerList :: [a] -> a
  -- (1, 2, 3)
  showerTuple :: [a] -> a
  -- "hello, (world)"
  showerStringLit :: String -> a
  -- '('
  showerCharLit :: Char -> a
  -- variable names, numeric literals, etc
  showerAtom :: String -> a
  -- whitespace-separated
  showerSpace :: [a] -> a
