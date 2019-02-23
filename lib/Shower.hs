-- | Pretty-print 'Show' output and JSON.
module Shower
  ( shower,
    showerString
  ) where

import Data.Bifunctor (bimap)
import Text.Megaparsec (parse, errorBundlePretty, eof)
import Shower.Parser (pShower)
import Shower.Printer (showerRender)

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
