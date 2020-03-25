{-# LANGUAGE OverloadedStrings #-}

module Data.Geometry.PolygonSpec where

import           Test.Hspec                          (Spec, describe, it,
                                                      shouldBe)

import qualified Data.Geometry.Clip.Internal.Polygon as InternalPolygon

import qualified Data.SpecHelper                     as SpecHelper

simplePolygonPts :: [(Double, Double)]
simplePolygonPts = [(0, 0), (4, 0), (4, 4), (0,4), (0,0)]

smallNegativePolyPts :: [(Double, Double)]
smallNegativePolyPts = [(3, 4), (5, 11), (12, 8), (9, 5), (5, 6), (3, 4)]

largeNegativePolyPts :: [(Double, Double)]
largeNegativePolyPts = [(3186, 2048), (3186, 2037), (3197, 2037), (3197, 2048), (3186, 2048)]

spec :: Spec
spec =
  testShoelace

testShoelace :: Spec
testShoelace = do
  describe "areas" $ do
    it "Small set of points 2" $
      InternalPolygon.surveyor (SpecHelper.listToSequenceGeo simplePolygonPts) `shouldBe` 16
    it "Small set of points" $
      InternalPolygon.surveyor (SpecHelper.listToSequenceGeo smallNegativePolyPts) `shouldBe` (-30)
    it "Large set of points" $
      InternalPolygon.surveyor (SpecHelper.listToSequenceGeo largeNegativePolyPts) `shouldBe` 121
  describe "winding order" $ do
    it "Small set of points 2" $
      InternalPolygon.isClockwise (SpecHelper.listToSequenceGeo simplePolygonPts) `shouldBe` False
    it "Small set of points" $
      InternalPolygon.isClockwise (SpecHelper.listToSequenceGeo smallNegativePolyPts) `shouldBe` True
    it "Large set of points" $
      InternalPolygon.isClockwise (SpecHelper.listToSequenceGeo largeNegativePolyPts) `shouldBe` False

