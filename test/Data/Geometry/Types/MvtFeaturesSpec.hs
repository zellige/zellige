{-# LANGUAGE OverloadedStrings #-}

module Data.Geometry.Types.MvtFeaturesSpec where

import qualified Data.Aeson                      as Aeson
import qualified Data.HashMap.Strict             as HashMapStrict
import qualified Data.Scientific                 as Scientific
import qualified Data.Sequence                   as Sequence
import qualified Data.Geometry.VectorTile.VectorTile            as VectorTile


import           Test.Hspec                      (Spec, describe, it, shouldBe)

import qualified Data.Geometry.Types.MvtFeatures as TypesMvtFeatures

spec :: Spec
spec = do
  testConvertProps
  testAddKeyValue

testConvertProps :: Spec
testConvertProps =
  describe "Simple" $
    it "Simple conversion of an Aeson value" $ do
      let testVals = Aeson.Object (HashMapStrict.fromList [("key1", Aeson.Number (Scientific.fromFloatDigits (1.0 :: Float))), ("key2", Aeson.String "string"), ("key3", Aeson.Bool True)])
          actual = TypesMvtFeatures.convertProps testVals
      actual `shouldBe` HashMapStrict.fromList [("key1", VectorTile.Do 1.0), ("key2", VectorTile.St "string"), ("key3", VectorTile.B True)]

testAddKeyValue :: Spec
testAddKeyValue =
  describe "Simple" $ do
    it "Simple add a new key value" $ do
      let expected = (1, HashMapStrict.fromList [(5,0)], Sequence.fromList [5])
          actual = TypesMvtFeatures.addKeyValue 0 (5 :: Int) HashMapStrict.empty Sequence.empty id
      actual `shouldBe` expected
    it "Add to an existing" $ do
      let expected = (2, HashMapStrict.fromList [(5,0), (6,1)], Sequence.fromList [5,6])
          actual = TypesMvtFeatures.addKeyValue 1 (6 :: Int) (HashMapStrict.fromList ([(5,0)] :: [(Int, Int)]))(Sequence.fromList ([5] :: [Int])) id
      actual `shouldBe` expected

