{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Property where

import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.UTF8 as UTF8

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
#else
import qualified Data.HashMap.Strict as HashMap
#endif

import Shower

-- | 'Aeson.Value' generator, stolen from @api-tools@.
genJSON :: Gen Aeson.Value
genJSON = sized $ \size -> oneof [
    Aeson.Object . objectFromList <$>
        resize (size `div` 2) (listOf ((,) <$> (keyFromText . Text.pack <$> arbitrary) <*> genJSON)),
    Aeson.Array . Vector.fromList <$> resize (size `div` 2) (listOf genJSON),
    Aeson.String . Text.pack <$> arbitrary,
    Aeson.Number . fromInteger <$> arbitrary,
    Aeson.Bool <$> arbitrary,
    pure Aeson.Null ]
  where
    (objectFromList, keyFromText) =
#if MIN_VERSION_aeson(2,0,0)
      (KeyMap.fromList, Key.fromText)
#else
      (HashMap.fromList, id)
#endif

-- | Test that formatting always works on JSON and never changes it.
prop_JSON :: Property
prop_JSON = forAll genJSON $ \original ->
  let reformatted =
        Aeson.eitherDecode . UTF8.fromString =<<
        showerString (UTF8.toString (Aeson.encode original))
  in reformatted === Right original

return []

-- | All property tests.
propertyTests :: TestTree
propertyTests = testProperties "" $(allProperties)
