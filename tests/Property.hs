{-# LANGUAGE TemplateHaskell #-}

module Property where

import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.UTF8 as UTF8

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

import Shower

-- | 'Aeson.Value' generator, stolen from @api-tools@.
genJSON :: Gen Aeson.Value
genJSON = sized $ \size -> oneof [
    Aeson.Object . HashMap.fromList <$>
        resize (size `div` 2) (listOf ((,) <$> (Text.pack <$> arbitrary) <*> genJSON)),
    Aeson.Array . Vector.fromList <$> resize (size `div` 2) (listOf genJSON),
    Aeson.String . Text.pack <$> arbitrary,
    Aeson.Number . fromInteger <$> arbitrary,
    Aeson.Bool <$> arbitrary,
    pure Aeson.Null ]

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
