{-# LANGUAGE OverloadedStrings #-}

module Data.Geometry.SphericalMercatorSpec where

import qualified Data.Aeson.Types                as AT
import qualified Data.Geospatial                 as Geospatial
import qualified Data.LinearRing                 as LinearRing
import qualified Data.LineString                 as LineString
import qualified Data.Vector                     as Vector
import qualified Data.Vector.Storable            as VectorStorable

import           Test.Hspec                      (Spec, describe, it, shouldBe)
import qualified Test.QuickCheck.Arbitrary       as QA
import qualified Test.QuickCheck.Gen             as GA

import           Data.Geometry.SphericalMercator
import           Data.Geometry.Types.Config

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

testLine :: Geospatial.GeospatialGeometry
testLine = Geospatial.Line (Geospatial.GeoLine (LineString.makeLineString (Geospatial._unGeoPoint pt1) (Geospatial._unGeoPoint pt2) VectorStorable.empty))

testPolygon :: Geospatial.GeospatialGeometry
testPolygon = Geospatial.Polygon (Geospatial.GeoPolygon (Vector.fromList [LinearRing.makeLinearRing (Geospatial._unGeoPoint pt1) (Geospatial._unGeoPoint pt2) (Geospatial._unGeoPoint pt3) Vector.empty]))

point1 :: Geospatial.GeospatialGeometry
point1 =  Geospatial.Point (Geospatial.GeoPoint (tupleToGeoPts (839.9996700223613, 2194.1081715809173)))

point2 :: Geospatial.GeospatialGeometry
point2 =  Geospatial.Point (Geospatial.GeoPoint (tupleToGeoPts (22.762837334737632, 2097.8526471037135)))

point3 :: Geospatial.GeospatialGeometry
point3 =  Geospatial.Point (Geospatial.GeoPoint (tupleToGeoPts (177.85887856088198, 1161.7239537991395)))

lineString :: Geospatial.GeospatialGeometry
lineString = Geospatial.Line (Geospatial.GeoLine (mkLineString (839.9996700223613, 2194.1081715809173) (22.762837334737632, 2097.8526471037135) []))

polygon :: Geospatial.GeospatialGeometry
polygon = Geospatial.Polygon (Geospatial.GeoPolygon (Vector.singleton $ mkLinearRing (839.9996700223613, 2194.1081715809173) (22.762837334737632, 2097.8526471037135) (177.85887856088198, 1161.7239537991395) []))

collection :: Geospatial.GeospatialGeometry
collection = Geospatial.Collection (Vector.fromList [point1, lineString, polygon])

mkFeatureID :: Word -> Maybe Geospatial.FeatureID
mkFeatureID = Just . Geospatial.FeatureIDNumber . fromIntegral

spec :: Spec
spec = do
  testConvertPoints
  testConvertLines
  testConvertPolygon
  testConvertCollection

testConvertPoints :: Spec
testConvertPoints =
  describe "point" $
    it "Returns values converted from 4326 to 3857 in a geojson feature" $ do
      x <- GA.generate QA.arbitrary :: IO Word
      let testFeature = Geospatial.GeoFeature Nothing (Geospatial.Point pt1) AT.Null (mkFeatureID x)
          actual = Vector.head $ convertFeatures (_zcExtents extentsBb) (_zcQuantize extentsBb) (_zcBBox extentsBb) (Vector.fromList [testFeature])
          expected = Geospatial.GeoFeature Nothing point1 AT.Null (mkFeatureID x)
      actual `shouldBe` expected

testConvertLines :: Spec
testConvertLines =
  describe "line" $
    it "Returns values converted from 4326 to 3857 in a geojson feature" $ do
      x <- GA.generate QA.arbitrary :: IO Word
      let testFeature = Geospatial.GeoFeature Nothing testLine AT.Null (mkFeatureID x)
          actual = Vector.head $ convertFeatures (_zcExtents extentsBb) (_zcQuantize extentsBb) (_zcBBox extentsBb) (Vector.fromList [testFeature])
          expected = Geospatial.GeoFeature Nothing lineString AT.Null (mkFeatureID x)
      actual `shouldBe` expected

testConvertPolygon :: Spec
testConvertPolygon =
  describe "polygon" $
    it "Returns values converted from 4326 to 3857 in a geojson feature" $ do
      x <- GA.generate QA.arbitrary :: IO Word
      let expected = Geospatial.GeoFeature Nothing polygon AT.Null (mkFeatureID x)
          feature = Geospatial.GeoFeature Nothing testPolygon AT.Null (mkFeatureID x)
          actual = Vector.head $ convertFeatures (_zcExtents extentsBb) (_zcQuantize extentsBb) (_zcBBox extentsBb) (Vector.fromList [feature])
      actual `shouldBe` expected

testConvertCollection :: Spec
testConvertCollection =
  describe "collection" $
    it "Returns values converted from 4326 to 3857 in a geojson feature" $ do
      x <- GA.generate QA.arbitrary :: IO Word
      let expected = Geospatial.GeoFeature Nothing collection AT.Null (mkFeatureID x)
          feature = Geospatial.GeoFeature Nothing (Geospatial.Collection (Vector.fromList [Geospatial.Point pt1, testLine, testPolygon])) AT.Null (mkFeatureID x)
          actual = convertFeatures (_zcExtents extentsBb) (_zcQuantize extentsBb) (_zcBBox extentsBb) (Vector.fromList [feature])
      actual `shouldBe` Vector.fromList [expected]
