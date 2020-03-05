{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.Geometry.GeoJsonToMvtSpec where

import qualified Control.Monad.ST                    as MonadST
import qualified Data.Aeson.Types                    as AesonTypes
import qualified Data.Geospatial                     as Geospatial
import qualified Data.HashMap.Strict                 as HashMapStrict
import qualified Data.LinearRing                     as LinearRing
import qualified Data.LineString                     as LineString
import qualified Data.Sequence                       as Sequence
import qualified Data.Geometry.VectorTile.VectorTile                as VectorTile


import           Test.Hspec                          (Spec, describe, it,
                                                      shouldBe)
import qualified Test.QuickCheck.Arbitrary           as QuickCheckArbitrary
import qualified Test.QuickCheck.Gen                 as QuickCheckGen

import qualified Data.Geometry.GeoJsonToMvt          as GeoJsonToMvt
import qualified Data.Geometry.SphericalMercator     as SphericalMercator
import qualified Data.Geometry.Types.Config          as Config
import qualified Data.Geometry.Types.GeoJsonFeatures as GeoJsonFeatures

import           Data.SpecHelper

config :: Config.Config
config = Config.mkConfig "foo" 18 (236629,160842) 128 2048 1 Config.NoAlgorithm

extentsBb :: Config.Config -> Config.ZoomConfig
extentsBb Config.Config{..} = Config.ZoomConfig _extents _quantizePixels (SphericalMercator.boundingBox _gtc) Config.NoAlgorithm

pt1 :: Geospatial.GeoPoint
pt1 = Geospatial.GeoPoint (Geospatial.GeoPointXY (Geospatial.PointXY 840 2194))

pt2 :: Geospatial.GeoPoint
pt2 = Geospatial.GeoPoint (Geospatial.GeoPointXY (Geospatial.PointXY 23 2098))

pt3 :: Geospatial.GeoPoint
pt3 = Geospatial.GeoPoint (Geospatial.GeoPointXY (Geospatial.PointXY 178 1162))

mkFeatureID :: Word -> Maybe Geospatial.FeatureID
mkFeatureID = Just . Geospatial.FeatureIDNumber . fromIntegral

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
      x <- QuickCheckGen.generate QuickCheckArbitrary.arbitrary :: IO Word
      let point = Geospatial.Point pt1
          feature = Geospatial.GeoFeature Nothing point AesonTypes.Null (mkFeatureID x)
          pts = tupleToPts [(840, 2194)]
          result = GeoJsonFeatures.MvtFeatures (Sequence.singleton $ VectorTile.Feature x HashMapStrict.empty pts) mempty mempty
          actual = MonadST.runST $ GeoJsonToMvt.geoJsonFeaturesToMvtFeatures GeoJsonFeatures.emptyMvtFeatures (Sequence.fromList [feature])
      actual `shouldBe` result

testLines :: Spec
testLines =
  describe "lines" $
    it "Returns mapnik lines feature from geojson feature" $ do
      x <- QuickCheckGen.generate QuickCheckArbitrary.arbitrary :: IO Word
      let feature = Geospatial.GeoFeature Nothing line AesonTypes.Null (mkFeatureID x)
          line = Geospatial.Line . Geospatial.GeoLine $ LineString.makeLineString (Geospatial._unGeoPoint pt1) (Geospatial._unGeoPoint pt2) Sequence.empty
          pts = tupleToPts [(840, 2194), (23, 2098)]
          result = GeoJsonFeatures.MvtFeatures mempty (Sequence.fromList [VectorTile.Feature x HashMapStrict.empty (Sequence.fromList [VectorTile.LineString pts])]) mempty
          actual = MonadST.runST $ GeoJsonToMvt.geoJsonFeaturesToMvtFeatures GeoJsonFeatures.emptyMvtFeatures (Sequence.fromList [feature])
      actual `shouldBe` result

testPolygons :: Spec
testPolygons =
  describe "polygons" $
    it "Returns mapnik polygon feature from geojson feature" $ do
      x <- QuickCheckGen.generate QuickCheckArbitrary.arbitrary :: IO Word
      let feature = Geospatial.GeoFeature Nothing polygon AesonTypes.Null (mkFeatureID x)
          polygon = Geospatial.Polygon . Geospatial.GeoPolygon $ Sequence.fromList [LinearRing.makeLinearRing (Geospatial._unGeoPoint pt1) (Geospatial._unGeoPoint pt2) (Geospatial._unGeoPoint pt3) mempty]
          pts = tupleToPts [(840, 2194), (23, 2098), (178, 1162), (840, 2194)]
          result = GeoJsonFeatures.MvtFeatures mempty mempty (Sequence.fromList [VectorTile.Feature x HashMapStrict.empty (Sequence.fromList [VectorTile.Polygon pts mempty])])
          actual = MonadST.runST $ GeoJsonToMvt.geoJsonFeaturesToMvtFeatures GeoJsonFeatures.emptyMvtFeatures (Sequence.fromList [feature])
      actual `shouldBe` result

testCounter :: Spec
testCounter =
  describe "features without id" $
    it "Returns same twice - tests counter" $ do
    let feature = Geospatial.GeoFeature Nothing point AesonTypes.Null Nothing
        point = Geospatial.Point pt1
        pts = tupleToPts [(840, 2194)]
        result = GeoJsonFeatures.MvtFeatures (Sequence.fromList [VectorTile.Feature 1 HashMapStrict.empty pts, VectorTile.Feature 2 HashMapStrict.empty pts]) mempty mempty
        actual = MonadST.runST $ GeoJsonToMvt.geoJsonFeaturesToMvtFeatures GeoJsonFeatures.emptyMvtFeatures (Sequence.fromList [feature, feature])
    actual `shouldBe` result
