{-# LANGUAGE OverloadedStrings #-}

module Data.Geometry.Types.MvtFeaturesSpec where

import qualified Data.Aeson                      as Aeson
import qualified Data.HashMap.Strict             as HashMapStrict
import qualified Data.Scientific                 as Scientific
import qualified Geography.VectorTile            as VectorTile

import           Test.Hspec                      (Spec, describe, it, shouldBe)

import qualified Data.Geometry.Types.MvtFeatures as TypesMvtFeatures

spec :: Spec
spec =
  testConvertProps

testConvertProps :: Spec
testConvertProps =
  describe "Simple" $
    it "Simple conversion of an Aeson value" $ do
      let testVals = Aeson.Object (HashMapStrict.fromList [("key1", Aeson.Number (Scientific.fromFloatDigits (1.0 :: Float))), ("key2", Aeson.String "string"), ("key3", Aeson.Bool True)])
          actual = TypesMvtFeatures.convertProps testVals
      actual `shouldBe` (HashMapStrict.fromList [("key1", VectorTile.Do 1.0), ("key2", VectorTile.St "string"), ("key3", VectorTile.B True)])

-- test
