{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.Geometry.GeoJsonToMvtSpec where

-- import qualified Control.Monad.ST                as ST
-- import qualified Data.Aeson                      as Aeson
-- import qualified Data.Aeson.Types                as AesonTypes
import qualified Data.Geospatial                 as Geospatial
-- import qualified Data.HashMap.Strict             as HashMapStrict
-- import qualified Data.LinearRing                 as LinearRing
-- import qualified Data.LineString                 as LineString
-- import qualified Data.Scientific                 as Scientific
-- import qualified Data.Text                       as Text
-- import qualified Data.Vector                     as Vector
-- import qualified Data.Vector.Storable            as VectorStorable
-- import qualified Geography.VectorTile            as VectorTile

-- import           Test.Hspec                      (Spec, describe, it, shouldBe)
import           Test.Hspec                      (Spec)
-- import qualified Test.QuickCheck.Arbitrary       as QA
-- import qualified Test.QuickCheck.Gen             as GA

import qualified Data.Geometry.SphericalMercator as SphericalMercator
import qualified Data.Geometry.Types.Config      as TypesConfig

config :: TypesConfig.Config
config = TypesConfig.mkConfig "foo" 18 (236629,160842) 128 2048 1 TypesConfig.NoAlgorithm

extentsBb :: TypesConfig.Config -> TypesConfig.ZoomConfig
extentsBb TypesConfig.Config{..} = TypesConfig.ZoomConfig _extents _quantizePixels (SphericalMercator.boundingBox _gtc) TypesConfig.NoAlgorithm

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

  -- convertProps :: Aeson.Value -> HashMapStrict.HashMap ByteStringLazy.ByteString VectorTile.Val

-- testPoints :: Spec
-- testPoints =
--   describe "points" $
--     it "Returns mapnik vector feature from geojson feature" $ do
--       x <- GA.generate QA.arbitrary :: IO Word
--       let point = Geospatial.Point pt1
--           feature = Geospatial.GeoFeature Nothing point AT.Null (mkFeatureID x)
--           pts = tupleToPts [(145, -38)]
--           result = TypesMvtFeatures.MvtFeatures (Vector.singleton $ VectorTile.Feature x HM.empty pts) mempty mempty
--           actual = ST.runST $ GeoJsonToMvt.geoJsonFeaturesToMvtFeatures TypesMvtFeatures.emptyMvtFeatures (Vector.fromList [feature])
--       actual `shouldBe` result

-- testLines :: Spec
-- testLines =
--   describe "lines" $
--     it "Returns mapnik lines feature from geojson feature" $ do
--       x <- GA.generate QA.arbitrary :: IO Word
--       let feature = Geospatial.GeoFeature Nothing line AT.Null (mkFeatureID x)
--           line = Geospatial.Line . Geospatial.GeoLine $ LineString.makeLineString (Geospatial._unGeoPoint pt1) (Geospatial._unGeoPoint pt2) VectorStorable.empty
--           pts = tupleToPts [(145, -38), (23, 2098)]
--           result = TypesMvtFeatures.MvtFeatures mempty (Vector.fromList [VectorTile.Feature x HM.empty (Vector.fromList [VectorTile.LineString pts])]) mempty
--           actual = ST.runST $ GeoJsonToMvt.geoJsonFeaturesToMvtFeatures TypesMvtFeatures.emptyMvtFeatures (Vector.fromList [feature])
--       actual `shouldBe` result

-- -- Add test when all points are removed from polygon.

-- testPolygons :: Spec
-- testPolygons =
--   describe "polygons" $
--     it "Returns mapnik polygon feature from geojson feature" $ do
--       x <- GA.generate QA.arbitrary :: IO Word
--       let feature = Geospatial.GeoFeature Nothing polygon AT.Null (mkFeatureID x)
--           polygon = Geospatial.Polygon . Geospatial.GeoPolygon $ Vector.fromList [LinearRing.makeLinearRing (Geospatial._unGeoPoint pt1) (Geospatial._unGeoPoint pt2) (Geospatial._unGeoPoint pt3) mempty]
--           pts = tupleToPts [(840, 2194), (23, 2098), (178, 1162), (840, 2194)]
--           result = TypesMvtFeatures.MvtFeatures mempty mempty (Vector.fromList [VectorTile.Feature x HM.empty (Vector.fromList [VectorTile.Polygon pts mempty])])
--           actual = ST.runST $ GeoJsonToMvt.geoJsonFeaturesToMvtFeatures TypesMvtFeatures.emptyMvtFeatures (Vector.fromList [feature])
--       actual `shouldBe` result

-- testCounter :: Spec
-- testCounter =
--   describe "features without id" $
--     it "Returns same twice - tests counter" $ do
--     let feature = Geospatial.GeoFeature Nothing point AT.Null Nothing
--         point = Geospatial.Point pt1
--         pts = tupleToPts [(840, 2194)]
--         result = TypesMvtFeatures.MvtFeatures (Vector.fromList [VectorTile.Feature 1 HM.empty pts, VectorTile.Feature 2 HM.empty pts]) mempty mempty
--         actual = ST.runST $ GeoJsonToMvt.geoJsonFeaturesToMvtFeatures TypesMvtFeatures.emptyMvtFeatures (Vector.fromList [feature, feature])
--     actual `shouldBe` result
