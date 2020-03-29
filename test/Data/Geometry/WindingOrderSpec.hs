{-# LANGUAGE OverloadedStrings #-}

module Data.Geometry.WindingOrderSpec where

import           Test.Hspec                 (Spec, describe, it, shouldBe)

import qualified Data.Geometry.WindingOrder as WindingOrder

import qualified Data.SpecHelper            as SpecHelper

simplePolygonPts :: [(Double, Double)]
simplePolygonPts = [(0, 0), (4, 0), (4, 4), (0, 4), (0, 0)]

smallNegativePolyPts :: [(Double, Double)]
smallNegativePolyPts = [(3, 4), (5, 11), (12, 8), (9, 5), (5, 6), (3, 4)]

largePositivePolyPts :: [(Double, Double)]
largePositivePolyPts = [(3186, 2048), (3186, 2037), (3197, 2037), (3197, 2048), (3186, 2048)]

rewoundSimplePolygonPts :: [(Double, Double)]
rewoundSimplePolygonPts = [(0, 0), (0, 4), (4, 4), (4, 0), (0,0)]

rewoundSmallNegativePolyPts :: [(Double, Double)]
rewoundSmallNegativePolyPts = [(3, 4), (5, 6), (9, 5), (12, 8), (5, 11), (3, 4)]

rewoundLargePositivePolyPts :: [(Double, Double)]
rewoundLargePositivePolyPts = [(3186, 2048), (3197, 2048), (3197, 2037), (3186, 2037), (3186, 2048)]

spec :: Spec
spec =
  testShoelace

testShoelace :: Spec
testShoelace = do
  describe "areas" $ do
    it "Small set of points 2" $
      WindingOrder.surveyor (SpecHelper.listToSequenceGeo simplePolygonPts) `shouldBe` 16
    it "Small set of points" $
      WindingOrder.surveyor (SpecHelper.listToSequenceGeo smallNegativePolyPts) `shouldBe` (-30)
    it "Large set of points" $
      WindingOrder.surveyor (SpecHelper.listToSequenceGeo largePositivePolyPts) `shouldBe` 121
  describe "winding order" $ do
    it "Small set of points 2" $
      WindingOrder.isClockwise (SpecHelper.listToSequenceGeo simplePolygonPts) `shouldBe` False
    it "Small set of points" $
      WindingOrder.isClockwise (SpecHelper.listToSequenceGeo smallNegativePolyPts) `shouldBe` True
    it "Large set of points" $
      WindingOrder.isClockwise (SpecHelper.listToSequenceGeo largePositivePolyPts) `shouldBe` False
  describe "rewind" $ do
    it "Small set of points 2" $
      WindingOrder.rewind (SpecHelper.listToSequenceGeo simplePolygonPts) `shouldBe` SpecHelper.listToSequenceGeo rewoundSimplePolygonPts
    it "Small set of points" $
      WindingOrder.rewind (SpecHelper.listToSequenceGeo smallNegativePolyPts) `shouldBe` SpecHelper.listToSequenceGeo rewoundSmallNegativePolyPts
    it "Large set of points" $
      WindingOrder.rewind (SpecHelper.listToSequenceGeo largePositivePolyPts) `shouldBe` SpecHelper.listToSequenceGeo rewoundLargePositivePolyPts
  describe "ensure order" $ do
    it "Small set of points 2" $
      WindingOrder.ensureOrder WindingOrder.Clockwise (SpecHelper.listToSequenceGeo simplePolygonPts) `shouldBe` SpecHelper.listToSequenceGeo rewoundSimplePolygonPts
    it "Large set of points" $
      WindingOrder.ensureOrder WindingOrder.AntiClockwise (SpecHelper.listToSequenceGeo largePositivePolyPts) `shouldBe` SpecHelper.listToSequenceGeo largePositivePolyPts
