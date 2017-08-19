{-# LANGUAGE OverloadedStrings #-}

module Data.Geometry.GeoJsonToMvtSpec where

import qualified Data.Aeson.Types                as AT
import qualified Data.Geography.GeoJSON          as GJ
import qualified Data.Map.Lazy                   as DMZ
import qualified Data.Scientific                 as S
import qualified Data.Vector                     as DV
import qualified Data.Vector.Unboxed             as DVU
import qualified Geography.VectorTile.Geometry   as VG
import qualified Geography.VectorTile.VectorTile as VVT
import           Test.Hspec                      (Spec, describe, it, shouldBe)
import qualified Test.QuickCheck.Arbitrary       as QA
import qualified Test.QuickCheck.Gen             as GA

import           Data.Geometry.GeoJsonToMvt
import           Data.Geometry.SphericalMercator
import           Data.Geometry.Types

config :: Config
config = mkConfig "foo" 18 (236629,160842) 128 2048

extentsBb :: (Pixels, BoundingBox)
extentsBb = (_extents config, boundingBox $ _gtc config)

pt1 :: GJ.PointGeometry
pt1 = GJ.PointGeometry [S.scientific 144961043 (-6), S.scientific (-37800096) (-6)]

pt2 :: GJ.PointGeometry
pt2 = GJ.PointGeometry [S.scientific 144960495 (-6), S.scientific (-37800045) (-6)]

pt3 :: GJ.PointGeometry
pt3 = GJ.PointGeometry [S.scientific 144960599 (-6), S.scientific (-37799549) (-6)]

spec :: Spec
spec = do
  testPoints
  testLines
  testPolygons
  testCounter

testPoints :: Spec
testPoints =
  describe "points" $ do
    it "Returns mapnik vector feature from geojson feature" $ do
      x <- GA.generate QA.arbitrary :: IO Int
      let feature = GJ.Feature Nothing point AT.Null (Just (AT.Number (fromIntegral x)))
          point = GJ.Point pt1
          pts = DV.fromList [(840,2194)]
          result = (DV.fromList [VVT.Feature x DMZ.empty pts], DV.empty, DV.empty)
      actual <- geoJsonFeaturesToMvtFeatures extentsBb [feature]
      actual `shouldBe` result

testLines :: Spec
testLines =
  describe "lines" $
    it "Returns mapnik lines feature from geojson feature" $ do
      x <- GA.generate QA.arbitrary :: IO Int
      let feature = GJ.Feature Nothing line AT.Null (Just (AT.Number (fromIntegral x)))
          line = GJ.LineString (GJ.LineStringGeometry [pt1, pt2])
          pts = DVU.fromList [(840,2194),(23,2098)]
          result = (DV.empty, DV.fromList [VVT.Feature x DMZ.empty (DV.fromList [VG.LineString pts])], DV.empty)
      actual <- geoJsonFeaturesToMvtFeatures extentsBb [feature]
      actual `shouldBe` result

testPolygons :: Spec
testPolygons =
  describe "polygons" $
    it "Returns mapnik polygon feature from geojson feature" $ do
      x <- GA.generate QA.arbitrary :: IO Int
      let feature = GJ.Feature Nothing polygon AT.Null (Just (AT.Number (fromIntegral x)))
          polygon = GJ.Polygon (GJ.PolygonGeometry [pt1, pt2, pt3] [])
          pts = DVU.fromList [(840,2194),(23,2098),(178,1162)]
          result = (DV.empty, DV.empty, DV.fromList [VVT.Feature x DMZ.empty (DV.fromList [VG.Polygon pts DV.empty])])
      actual <- geoJsonFeaturesToMvtFeatures extentsBb [feature]
      actual `shouldBe` result

testCounter :: Spec
testCounter =
  describe "features without id" $
    it "Returns same twice - tests counter" $ do
    let feature = GJ.Feature Nothing point AT.Null Nothing
        point = GJ.Point pt1
        pts = DV.fromList [(840,2194)]
        result = (DV.fromList [VVT.Feature 1 DMZ.empty pts, VVT.Feature 2 DMZ.empty pts], DV.empty, DV.empty)
    actual <- geoJsonFeaturesToMvtFeatures extentsBb [feature, feature]
    actual `shouldBe` result
