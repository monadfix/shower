module Shower
  ( shower,
    showerString
  ) where

import Data.Bifunctor (bimap)
import Text.Megaparsec (parse, errorBundlePretty, eof)
import Shower.Parser (pShower)
import Shower.Printer (showerRender)

shower :: Show a => a -> String
shower a =
  case showerString s of
    Left _ -> s
    Right s' -> s'
  where
    s = show a

showerString :: String -> Either String String
showerString s =
  bimap errorBundlePretty showerRender (parse (pShower <* eof) "" s)
