{-# LANGUAGE OverloadedStrings #-}

import Shower
import Reflex.Dom
import qualified Data.Text as Text

main :: IO ()
main = mainWidgetInElementById "shower-demo" $ do
    t <- textArea $ def
        & attributes .~ constDyn ("placeholder" =: "Paste your horrible 'Show' output here.")
    el "pre" $
        el "code" $
            dynText (Text.pack . ppr . Text.unpack <$> _textArea_value t)

ppr :: String -> String
ppr "" = ""
ppr s =
  case showerString s of
    Left e -> "Could not prettify the provided source:\n" ++ e
    Right r -> r
