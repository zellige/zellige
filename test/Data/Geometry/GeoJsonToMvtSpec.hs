{-# LANGUAGE OverloadedStrings #-}

module Data.Geometry.GeoJsonToMvtSpec where

import qualified Control.Monad.ST                as ST
import qualified Data.Aeson.Types                as AT
import qualified Data.Geospatial                 as GJ
import qualified Data.HashMap.Strict             as HM
import qualified Data.LinearRing                 as GJ
import qualified Data.LineString                 as GJ
import qualified Data.Vector                     as Vector
import qualified Geography.VectorTile            as VG

import           Test.Hspec                      (Spec, describe, it, shouldBe)
import qualified Test.QuickCheck.Arbitrary       as QA
import qualified Test.QuickCheck.Gen             as GA

import           Data.Geometry.GeoJsonToMvt
import           Data.Geometry.SphericalMercator
import           Data.Geometry.Types.MvtFeatures
import           Data.Geometry.Types.Simplify
import           Data.Geometry.Types.Types

import           Data.Geometry.SpecHelper

config :: Config
config = mkConfig "foo" 18 (236629,160842) 128 2048 1 NoAlgorithm

extentsBb :: ZoomConfig
extentsBb = ZoomConfig (_extents config) (_quantizePixels config) (boundingBox $ _gtc config) NoAlgorithm

pt1 :: GJ.GeoPoint
pt1 = GJ.GeoPoint [144.961043, -37.800096]

pt2 :: GJ.GeoPoint
pt2 = GJ.GeoPoint [144.960495, -37.800045]

pt3 :: GJ.GeoPoint
pt3 = GJ.GeoPoint [144.960599, -37.799549]

mkFeatureID :: Word -> Maybe GJ.FeatureID
mkFeatureID = Just . GJ.FeatureIDNumber . fromIntegral

spec :: Spec
spec = do
  testPoints
  testLines
  testPolygons
  testCounter

testPoints :: Spec
testPoints =
  describe "points" $
    it "Returns mapnik vector feature from geojson feature" $ do
      x <- GA.generate QA.arbitrary :: IO Word
      let feature = GJ.GeoFeature Nothing point AT.Null (mkFeatureID x)
          point = GJ.Point pt1
          pts = tupleToPts [(840,2194)]
          result = MvtFeatures (Vector.singleton $ VG.Feature x HM.empty pts) mempty mempty
          actual = ST.runST $ geoJsonFeaturesToMvtFeatures extentsBb [feature]
      actual `shouldBe` result

testLines :: Spec
testLines =
  describe "lines" $
    it "Returns mapnik lines feature from geojson feature" $ do
      x <- GA.generate QA.arbitrary :: IO Word
      let feature = GJ.GeoFeature Nothing line AT.Null (mkFeatureID x)
          line = GJ.Line . GJ.GeoLine $ GJ.makeLineString (GJ._unGeoPoint pt1) (GJ._unGeoPoint pt2) []
          pts = tupleToPts [(840,2194),(23,2098)]
          result = MvtFeatures mempty (Vector.fromList [VG.Feature x HM.empty (Vector.fromList [VG.LineString pts])]) mempty
          actual = ST.runST $ geoJsonFeaturesToMvtFeatures extentsBb [feature]
      actual `shouldBe` result

-- Add test when all points are removed from polygon.

testPolygons :: Spec
testPolygons =
  describe "polygons" $
    it "Returns mapnik polygon feature from geojson feature" $ do
      x <- GA.generate QA.arbitrary :: IO Word
      let feature = GJ.GeoFeature Nothing polygon AT.Null (mkFeatureID x)
          polygon = GJ.Polygon . GJ.GeoPolygon $ [GJ.makeLinearRing (GJ._unGeoPoint pt1) (GJ._unGeoPoint pt2) (GJ._unGeoPoint pt3) []]
          pts = tupleToPts [(840,2194), (23,2098), (178,1162), (840,2194)]
          result = MvtFeatures mempty mempty (Vector.fromList [VG.Feature x HM.empty (Vector.fromList [VG.Polygon pts mempty])])
          actual = ST.runST $ geoJsonFeaturesToMvtFeatures extentsBb [feature]
      actual `shouldBe` result

testCounter :: Spec
testCounter =
  describe "features without id" $
    it "Returns same twice - tests counter" $ do
    let feature = GJ.GeoFeature Nothing point AT.Null Nothing
        point = GJ.Point pt1
        pts = tupleToPts [(840,2194)]
        result = MvtFeatures (Vector.fromList [VG.Feature 1 HM.empty pts, VG.Feature 2 HM.empty pts]) mempty mempty
        actual = ST.runST $ geoJsonFeaturesToMvtFeatures extentsBb [feature, feature]
    actual `shouldBe` result
