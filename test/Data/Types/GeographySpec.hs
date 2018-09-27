{-# LANGUAGE OverloadedStrings #-}

module Data.Types.GeographySpec where

import qualified Data.Geospatial               as Geospatial
import qualified Data.Sequence                 as Sequence
import           Test.Hspec                    (Spec, describe, it, shouldBe)

import qualified Data.Geometry.Types.Geography as GeometryGeography

import qualified Data.SpecHelper               as SpecHelper

spec :: Spec
spec =
  testPointsToLines

testPointsToLines :: Spec
testPointsToLines =
  describe "pointsToLines" $ do
    it "Simple test" $ do
      let inputPts = SpecHelper.listToSequenceGeo [(1,2),(2,3),(3,4)]
          expectedLines = SpecHelper.listToSequenceGeoLine [((3,4),(1,2)),((1,2),(2,3)),((2,3),(3,4))]
      expectedLines `shouldBe` GeometryGeography.pointsToLines inputPts
    it "Empty Test" $
      Sequence.empty `shouldBe` GeometryGeography.pointsToLines (Sequence.empty :: Sequence.Seq Geospatial.PointXY)
    it "Single element test" $
      Sequence.empty `shouldBe` GeometryGeography.pointsToLines (Sequence.fromList ([] :: [Geospatial.PointXY]))
