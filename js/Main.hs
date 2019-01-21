{-# LANGUAGE OverloadedStrings #-}

import Shower
import Reflex.Dom
import qualified Data.Text as Text

main :: IO ()
main = mainWidget $ el "div" $ do
    t <- textArea def
    el "pre" (dynText (Text.pack . ppr . Text.unpack <$> _textArea_value t))

ppr :: String -> String
ppr s =
  case showerString s of
    Left e -> "Could not prettify the provided source:\n" ++ e
    Right r -> r
