{-# LANGUAGE OverloadedStrings #-}

module Data.Geometry.GeoJsonToMvtSpec where

import qualified Control.Monad.ST                as ST
import qualified Data.Aeson.Types                as AT
import qualified Data.Geospatial                 as Geospatial
import qualified Data.HashMap.Strict             as HM
import qualified Data.LinearRing                 as LinearRing
import qualified Data.LineString                 as LineString
import qualified Data.Vector                     as Vector
import qualified Data.Vector.Storable            as VectorStorable
import qualified Geography.VectorTile            as VectorTile

import           Test.Hspec                      (Spec, describe, it, shouldBe)
import qualified Test.QuickCheck.Arbitrary       as QA
import qualified Test.QuickCheck.Gen             as GA

import           Data.Geometry.GeoJsonToMvt
import           Data.Geometry.SphericalMercator
import           Data.Geometry.Types.Config
import qualified Data.Geometry.Types.MvtFeatures as MvtFeatures

import           Data.Geometry.SpecHelper

config :: Config
config = mkConfig "foo" 18 (236629,160842) 128 2048 1 NoAlgorithm

extentsBb :: ZoomConfig
extentsBb = ZoomConfig (_extents config) (_quantizePixels config) (boundingBox $ _gtc config) NoAlgorithm

pt1 :: Geospatial.GeoPoint
pt1 = Geospatial.GeoPoint (Geospatial.GeoPointXY (Geospatial.PointXY 144.961043 (-37.800096)))

pt2 :: Geospatial.GeoPoint
pt2 = Geospatial.GeoPoint (Geospatial.GeoPointXY (Geospatial.PointXY 144.960495 (-37.800045)))

pt3 :: Geospatial.GeoPoint
pt3 = Geospatial.GeoPoint (Geospatial.GeoPointXY (Geospatial.PointXY 144.960599 (-37.799549)))

mkFeatureID :: Word -> Maybe Geospatial.FeatureID
mkFeatureID = Just . Geospatial.FeatureIDNumber . fromIntegral

spec :: Spec
spec = pure ()
  -- testPoints
  -- testLines
  -- testPolygons
  -- testCounter

testPoints :: Spec
testPoints =
  describe "points" $
    it "Returns mapnik vector feature from geojson feature" $ do
      x <- GA.generate QA.arbitrary :: IO Word
      let point = Geospatial.Point pt1
          feature = Geospatial.GeoFeature Nothing point AT.Null (mkFeatureID x)
          pts = tupleToPts [(840, 2194)]
          result = MvtFeatures.MvtFeatures (Vector.singleton $ VectorTile.Feature x HM.empty pts) mempty mempty
          actual = ST.runST $ geoJsonFeaturesToMvtFeatures MvtFeatures.emptyMvtFeatures (Vector.fromList [feature])
      actual `shouldBe` result

testLines :: Spec
testLines =
  describe "lines" $
    it "Returns mapnik lines feature from geojson feature" $ do
      x <- GA.generate QA.arbitrary :: IO Word
      let feature = Geospatial.GeoFeature Nothing line AT.Null (mkFeatureID x)
          line = Geospatial.Line . Geospatial.GeoLine $ LineString.makeLineString (Geospatial._unGeoPoint pt1) (Geospatial._unGeoPoint pt2) VectorStorable.empty
          pts = tupleToPts [(840, 2194), (23, 2098)]
          result = MvtFeatures.MvtFeatures mempty (Vector.fromList [VectorTile.Feature x HM.empty (Vector.fromList [VectorTile.LineString pts])]) mempty
          actual = ST.runST $ geoJsonFeaturesToMvtFeatures MvtFeatures.emptyMvtFeatures (Vector.fromList [feature])
      actual `shouldBe` result

-- Add test when all points are removed from polygon.

testPolygons :: Spec
testPolygons =
  describe "polygons" $
    it "Returns mapnik polygon feature from geojson feature" $ do
      x <- GA.generate QA.arbitrary :: IO Word
      let feature = Geospatial.GeoFeature Nothing polygon AT.Null (mkFeatureID x)
          polygon = Geospatial.Polygon . Geospatial.GeoPolygon $ Vector.fromList [LinearRing.makeLinearRing (Geospatial._unGeoPoint pt1) (Geospatial._unGeoPoint pt2) (Geospatial._unGeoPoint pt3) Vector.empty]
          pts = tupleToPts [(840, 2194), (23, 2098), (178, 1162), (840, 2194)]
          result = MvtFeatures.MvtFeatures mempty mempty (Vector.fromList [VectorTile.Feature x HM.empty (Vector.fromList [VectorTile.Polygon pts mempty])])
          actual = ST.runST $ geoJsonFeaturesToMvtFeatures MvtFeatures.emptyMvtFeatures (Vector.fromList [feature])
      actual `shouldBe` result

testCounter :: Spec
testCounter =
  describe "features without id" $
    it "Returns same twice - tests counter" $ do
    let feature = Geospatial.GeoFeature Nothing point AT.Null Nothing
        point = Geospatial.Point pt1
        pts = tupleToPts [(840, 2194)]
        result = MvtFeatures.MvtFeatures (Vector.fromList [VectorTile.Feature 1 HM.empty pts, VectorTile.Feature 2 HM.empty pts]) mempty mempty
        actual = ST.runST $ geoJsonFeaturesToMvtFeatures MvtFeatures.emptyMvtFeatures (Vector.fromList [feature, feature])
    actual `shouldBe` result
