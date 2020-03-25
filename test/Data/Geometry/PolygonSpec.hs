{-# LANGUAGE OverloadedStrings #-}

module Data.Geometry.PolygonSpec where

import           Test.Hspec                          (Spec, describe, it,
                                                      shouldBe)

import qualified Data.Geometry.Clip.Internal.Polygon as InternalPolygon

import qualified Data.SpecHelper                     as SpecHelper

simplePolygonPts :: [(Double, Double)]
simplePolygonPts = [(0, 0), (4, 0), (4, 4), (0, 4)]

expectedSimplePolygonPts :: [(Double, Double)]
expectedSimplePolygonPts = [(0, 0), (4, 0), (4, 4), (0, 4), (0,0)]

spec :: Spec
spec =
  testCloseIfNot

testCloseIfNot :: Spec
testCloseIfNot =
  describe "close" $
    it "Small set of points" $
      InternalPolygon.closeIfNot (SpecHelper.listToSequenceGeo simplePolygonPts) `shouldBe` Just (SpecHelper.listToSequenceGeo expectedSimplePolygonPts)
