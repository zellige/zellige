{-# LANGUAGE OverloadedStrings #-}

module Data.Geometry.GeoJsonToMvtSpec where

import qualified Control.Monad.ST                as ST
import qualified Data.Aeson.Types                as AT
import qualified Data.Geospatial                 as GJ
import qualified Data.LinearRing                 as GJ
import qualified Data.LineString                 as GJ
import qualified Data.Map.Lazy                   as DMZ
import qualified Data.Sequence                   as DS
import qualified Data.Vector                     as DV
import qualified Data.Vector.Unboxed             as DVU
import qualified Geography.VectorTile.Geometry   as VG
import qualified Geography.VectorTile.VectorTile as VVT

import           Test.Hspec                      (Spec, describe, it, shouldBe)
import qualified Test.QuickCheck.Arbitrary       as QA
import qualified Test.QuickCheck.Gen             as GA

import           Data.Geometry.GeoJsonToMvt
import           Data.Geometry.SphericalMercator
import           Data.Geometry.Types.MvtFeatures
import           Data.Geometry.Types.Types

config :: Config
config = mkConfig "foo" 18 (236629,160842) 128 2048 1

extentsBb :: ZoomConfig
extentsBb = ZoomConfig (_extents config) (_quantizePixels config) (boundingBox $ _gtc config)

pt1 :: GJ.GeoPoint
pt1 = GJ.GeoPoint [144.961043, -37.800096]

pt2 :: GJ.GeoPoint
pt2 = GJ.GeoPoint [144.960495, -37.800045]

pt3 :: GJ.GeoPoint
pt3 = GJ.GeoPoint [144.960599, -37.799549]

mkFeatureID :: Int -> Maybe GJ.FeatureID
mkFeatureID = Just . GJ.FeatureIDNumber

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
      x <- GA.generate QA.arbitrary :: IO Int
      let feature = GJ.GeoFeature Nothing point AT.Null (mkFeatureID x)
          point = GJ.Point pt1
          pts = DV.fromList [(840,2194)]
          result = MvtFeatures (DS.singleton $ VVT.Feature x DMZ.empty pts) mempty mempty
          actual = ST.runST $ geoJsonFeaturesToMvtFeatures extentsBb [feature]
      actual `shouldBe` result

testLines :: Spec
testLines =
  describe "lines" $
    it "Returns mapnik lines feature from geojson feature" $ do
      x <- GA.generate QA.arbitrary :: IO Int
      let feature = GJ.GeoFeature Nothing line AT.Null (mkFeatureID x)
          line = GJ.Line . GJ.GeoLine $ GJ.makeLineString (GJ._unGeoPoint pt1) (GJ._unGeoPoint pt2) []
          pts = DVU.fromList [(840,2194),(23,2098)]
          result = MvtFeatures mempty (DS.fromList [VVT.Feature x DMZ.empty (DV.fromList [VG.LineString pts])]) mempty
          actual = ST.runST $ geoJsonFeaturesToMvtFeatures extentsBb [feature]
      actual `shouldBe` result

-- Add test when all points are removed from polygon.

testPolygons :: Spec
testPolygons =
  describe "polygons" $
    it "Returns mapnik polygon feature from geojson feature" $ do
      x <- GA.generate QA.arbitrary :: IO Int
      let feature = GJ.GeoFeature Nothing polygon AT.Null (mkFeatureID x)
          polygon = GJ.Polygon . GJ.GeoPolygon $ [GJ.makeLinearRing (GJ._unGeoPoint pt1) (GJ._unGeoPoint pt2) (GJ._unGeoPoint pt3) []]
          pts = DVU.fromList [(840,2194), (23,2098), (178,1162), (840,2194)]
          result = MvtFeatures mempty mempty (DS.fromList [VVT.Feature x DMZ.empty (DV.fromList [VG.Polygon pts DV.empty])])
          actual = ST.runST $ geoJsonFeaturesToMvtFeatures extentsBb [feature]
      actual `shouldBe` result

testCounter :: Spec
testCounter =
  describe "features without id" $
    it "Returns same twice - tests counter" $ do
    let feature = GJ.GeoFeature Nothing point AT.Null Nothing
        point = GJ.Point pt1
        pts = DV.fromList [(840,2194)]
        result = MvtFeatures (DS.fromList [VVT.Feature 1 DMZ.empty pts, VVT.Feature 2 DMZ.empty pts]) mempty mempty
        actual = ST.runST $ geoJsonFeaturesToMvtFeatures extentsBb [feature, feature]
    actual `shouldBe` result
