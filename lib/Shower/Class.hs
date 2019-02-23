{- |

This module defines the representation of data that the parser produces and the
pretty-printer consumes.

-}
module Shower.Class where

-- | A tagless final encoding for a result builder (@ShowS@, @Doc@, @Html@, etc).
--
-- Note that 'showerStringLit' and 'showerCharLit' take exact uninterpreted
-- strings to avoid losing information (e.g. @"\\n"@ vs. @"\\10"@).
class Shower a where
  -- | A record, @{ x = 24, y = 42 }@ or @{ "a": null, "b": 13 }@.
  showerRecord :: [ShowerComma (a, ShowerFieldSep, a)] -> a
  -- | A list, @[1, 2, 3]@.
  showerList :: [ShowerComma a] -> a
  -- | A tuple, @(1, 2, 3)@.
  showerTuple :: [ShowerComma a] -> a
  -- | A string literal, @"hello, (world)"@.
  showerStringLit :: String -> a
  -- | A character literal, @'('@.
  showerCharLit :: String -> a
  -- | Variable names, numeric literals, and so on.
  showerAtom :: String -> a
  -- | Whitespace-separated elements.
  showerSpace :: [a] -> a

-- | A field separator used in records, either @\'=\'@ for Haskell records or
-- @\':\'@ for JSON.
data ShowerFieldSep =
  ShowerFieldSepEquals {- ^ An equality sign, @\'=\'@ -} |
  ShowerFieldSepColon  {- ^ A colon, @\':\'@ -}

{- | Either a comma or an element.

For example, the tuple section @(,a,,b)@ is represented like this:

@
[ ShowerCommaSep,
  ShowerCommaElement "a",
  ShowerCommaSep,
  ShowerCommaSep,
  ShowerCommaElement "b" ]
@
-}
data ShowerComma a =
  ShowerCommaSep {- ^ A comma, @\',\'@ -} |
  ShowerCommaElement a {- ^ An element -}
