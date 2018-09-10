{-# LANGUAGE OverloadedStrings #-}

module Data.Geometry.SphericalMercatorSpec where

import qualified Data.Aeson.Types                as AT
import qualified Data.Geospatial                 as Geospatial
import qualified Data.LineString                 as LineString
import qualified Data.Vector                     as Vector

import           Test.Hspec                      (Spec, describe, it, shouldBe)
import qualified Test.QuickCheck.Arbitrary       as QA
import qualified Test.QuickCheck.Gen             as GA

import           Data.Geometry.SphericalMercator
import           Data.Geometry.Types.Config
import           Data.Geometry.Types.Simplify

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
spec = do
  testPoints
  testLines

testPoints :: Spec
testPoints =
  describe "points" $
    it "Returns values converted from 4326 to 3857 in a geojson feature" $ do
      x <- GA.generate QA.arbitrary :: IO Word
      let pts = Geospatial.Point . Geospatial.GeoPoint $ tupleToGeoPts (840, 2194)
          expected = Geospatial.GeoFeature Nothing pts AT.Null (mkFeatureID x)
          point = Geospatial.Point pt1
          feature = Geospatial.GeoFeature Nothing point AT.Null (mkFeatureID x)
          actual = Vector.head $ convertFeatures (_zcExtents extentsBb) (_zcQuantize extentsBb) (_zcBBox extentsBb) (Vector.fromList [feature])
      actual `shouldBe` expected

testLines :: Spec
testLines =
  describe "lines" $
    it "Returns values converted from 4326 to 3857 in a geojson feature" $ do
      x <- GA.generate QA.arbitrary :: IO Word
      let lineString = Geospatial.Line $ Geospatial.GeoLine (mkLineString (840, 2194) (23, 2098) [])
          expected = Geospatial.GeoFeature Nothing lineString AT.Null (mkFeatureID x)
          line = Geospatial.Line . Geospatial.GeoLine $ LineString.makeLineString (Geospatial._unGeoPoint pt1) (Geospatial._unGeoPoint pt2) []
          feature = Geospatial.GeoFeature Nothing line AT.Null (mkFeatureID x)
          actual = Vector.head $ convertFeatures (_zcExtents extentsBb) (_zcQuantize extentsBb) (_zcBBox extentsBb) (Vector.fromList [feature])
      actual `shouldBe` expected

-- -- Add test when all points are removed from polygon.

-- testPolygons :: Spec
-- testPolygons =
--   describe "polygons" $
--     it "Returns mapnik polygon feature from geojson feature" $ do
--       x <- GA.generate QA.arbitrary :: IO Word
--       let feature = Geospatial.GeoFeature Nothing polygon AT.Null (mkFeatureID x)
--           polygon = Geospatial.Polygon . Geospatial.GeoPolygon $ Vector.fromList [LinearRing.makeLinearRing (Geospatial._unGeoPoint pt1) (Geospatial._unGeoPoint pt2) (Geospatial._unGeoPoint pt3) []]
--           pts = tupleToPts [(840, 2194), (23, 2098), (178, 1162), (840, 2194)]
--           result = MvtFeatures.MvtFeatures mempty mempty (Vector.fromList [VectorTile.Feature x HM.empty (Vector.fromList [VectorTile.Polygon pts mempty])])
--           actual = ST.runST $ geoJsonFeaturesToMvtFeatures MvtFeatures.emptyMvtFeatures (Vector.fromList [feature])
--       actual `shouldBe` result

-- testCounter :: Spec
-- testCounter =
--   describe "features without id" $
--     it "Returns same twice - tests counter" $ do
--     let feature = Geospatial.GeoFeature Nothing point AT.Null Nothing
--         point = Geospatial.Point pt1
--         pts = tupleToPts [(840, 2194)]
--         result = MvtFeatures.MvtFeatures (Vector.fromList [VectorTile.Feature 1 HM.empty pts, VectorTile.Feature 2 HM.empty pts]) mempty mempty
--         actual = ST.runST $ geoJsonFeaturesToMvtFeatures MvtFeatures.emptyMvtFeatures (Vector.fromList [feature, feature])
--     actual `shouldBe` result
