-- | Pretty-print 'Show' output and JSON.
module Shower
  ( shower,
    printer,
    showerString
  ) where

import Data.Bifunctor (bimap)
import Text.Megaparsec (parse, errorBundlePretty, eof)
import Shower.Parser (pShower)
import Shower.Printer (showerRender)

-- | A drop-in replacement for @print@ that has nice layout.
-- Use it in GHCi with @-interactive-print=Shower.printer@:
--
-- @
-- ghci> :set -interactive-print=Shower.printer
-- ghci> ([1..15], ['a'..'z'])
-- ([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15],
--  "abcdefghijklmnopqrstuvwxyz")
-- @
--
-- NB: does not handle infinite data structures at the moment.
printer :: Show a => a -> IO ()
printer a = putStrLn (shower a)

-- | A drop-in replacement for @show@ that has nice layout.
-- NB: does not handle infinite data structures at the moment.
shower :: Show a => a -> String
shower a =
  case showerString s of
    Left _ -> s
    Right s' -> s'
  where
    s = show a

-- | Parse and pretty-print 'show' output or @JSON@.
showerString :: String -> Either String String
showerString s =
  bimap errorBundlePretty showerRender (parse (pShower <* eof) "" s)
