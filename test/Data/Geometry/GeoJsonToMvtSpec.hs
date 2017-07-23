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
  testFeatureToPoints
  testFeatureToLines
  testFeatureToPolygon

testFeatureToPoints :: Spec
testFeatureToPoints =
  describe "featureToPoints" $
    it "Returns points from a feature" $ do
      let feature = GJ.Feature Nothing point AT.Null Nothing
          point = GJ.Point pt1
          actual = geoJsonFeaturesToMvtFeatures extentsBb [feature]
          pts = DV.fromList [(840,2194)]
          result = (DV.fromList [VVT.Feature 0 DMZ.empty pts], DV.empty, DV.empty)
      actual `shouldBe` result

testFeatureToLines :: Spec
testFeatureToLines =
  describe "featureToLine" $
    it "Returns line from a feature" $ do
      let feature = GJ.Feature Nothing line AT.Null Nothing
          line = GJ.LineString (GJ.LineStringGeometry [pt1, pt2])
          actual = geoJsonFeaturesToMvtFeatures extentsBb [feature]
          pts = DVU.fromList [(840,2194),(23,2098)]
          result = (DV.empty, DV.fromList [VVT.Feature 0 DMZ.empty (DV.fromList [VG.LineString pts])], DV.empty)
      actual `shouldBe` result

testFeatureToPolygon :: Spec
testFeatureToPolygon =
  describe "featureToPolygon" $
    it "Returns polygon from a feature" $ do
      let feature = GJ.Feature Nothing polygon AT.Null Nothing
          polygon = GJ.Polygon (GJ.PolygonGeometry [pt1, pt2, pt3] [])
          actual = geoJsonFeaturesToMvtFeatures extentsBb [feature]
          pts = DVU.fromList [(840,2194),(23,2098),(178,1162)]
          result = (DV.empty, DV.empty, DV.fromList [VVT.Feature 0 DMZ.empty (DV.fromList [VG.Polygon pts DV.empty])])
      actual `shouldBe` result
