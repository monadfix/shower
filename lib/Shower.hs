module Shower
  ( shower,
    showerString
  ) where

import Text.Megaparsec (parse, errorBundlePretty)
import Shower.Parser (pShower)
import Shower.Printer (showerRender)

shower :: Show a => a -> String
shower = showerString . show

showerString :: String -> String
showerString s =
  either (error . errorBundlePretty) showerRender (parse pShower "" s)
