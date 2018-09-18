{-# LANGUAGE OverloadedStrings #-}

module Data.Geometry.ClipSpec where

import qualified Data.Aeson                    as Aeson
import qualified Data.Geospatial               as Geospatial
import qualified Data.LinearRing               as LinearRing
import qualified Data.Vector                   as Vector
import qualified Geography.VectorTile          as VectorTile
import           Test.Hspec                    (Spec, describe, it, shouldBe)

import qualified Data.Geometry.Clip            as GeometryClip
import qualified Data.Geometry.Types.Geography as GeometryGeography

import qualified Data.Geometry.SpecHelper      as SpecHelper

polyPts :: [(Int, Int)]
polyPts = [ (50,150),  (200, 50)
          , (350,150), (350,300)
          , (250,300), (200,250)
          , (150,350), (100,250)
          , (100,200)
          ]

innerPolyPts :: [(Int, Int)]
innerPolyPts = [(75,200),(250,250),(250,150),(75,150)]

poly :: VectorTile.Polygon
poly = VectorTile.Polygon (SpecHelper.tupleToPts polyPts) mempty

lineClipPts :: GeometryGeography.BoundingBoxPts
lineClipPts = GeometryGeography.BoundingBoxPts (VectorTile.Point 10 10) (VectorTile.Point 60 60)

linesTst :: Vector.Vector VectorTile.LineString
linesTst = Vector.fromList
  [ VectorTile.LineString (SpecHelper.tupleToPts [(11, 11), (59, 59)])
  , VectorTile.LineString (SpecHelper.tupleToPts [(0, 0), (0, 100)])
  , VectorTile.LineString (SpecHelper.tupleToPts [(5, 5), (45, 50), (90, 140)])
  , VectorTile.LineString (SpecHelper.tupleToPts [(0, 0), (10, 10)])
  , VectorTile.LineString (SpecHelper.tupleToPts [(50, 50), (0, 10)])
  , VectorTile.LineString (SpecHelper.tupleToPts [(0, 0), (60, 60)])]

resultLines :: Vector.Vector VectorTile.LineString
resultLines = Vector.fromList
  [ VectorTile.LineString (SpecHelper.tupleToPts [(10, 10), (60, 60)])
  , VectorTile.LineString (SpecHelper.tupleToPts [(50, 50), (10, 18)])
  , VectorTile.LineString (SpecHelper.tupleToPts [(10, 10), (10, 10)])
  , VectorTile.LineString (SpecHelper.tupleToPts [(10, 11), (45, 50), (50, 60)])
  , VectorTile.LineString (SpecHelper.tupleToPts [(11, 11), (59, 59)])
  ]

resultPolyPts :: [(Int, Int)]
resultPolyPts = [(100,200),(100,116),(124,100),(275,100),(300,116),(300,300),(250,300),(200,250),(175,300),(125,300),(100,250),(100,200)]

innerPolyResultPts :: [(Int, Int)]
innerPolyResultPts = [(100,150),(100,207),(250,250),(250,150),(100,150)]

resultPolyWithInner :: VectorTile.Polygon
resultPolyWithInner = VectorTile.Polygon (SpecHelper.tupleToPts resultPolyPts) (Vector.fromList [VectorTile.Polygon (SpecHelper.tupleToPts innerPolyResultPts) mempty])

geoLineTst :: Geospatial.GeoLine
geoLineTst = Geospatial.GeoLine (SpecHelper.mkLineString (5, 5) (45, 50) [(90, 140)])

geoLinesTst :: Geospatial.GeoMultiLine
geoLinesTst = Geospatial.GeoMultiLine $ Vector.fromList
  [ SpecHelper.mkLineString (11, 11) (59, 59) []
  , SpecHelper.mkLineString (0, 0) (0, 100) []
  , SpecHelper.mkLineString (5, 5) (45, 50) [(90, 140)]
  , SpecHelper.mkLineString (0, 0) (10, 10) []
  , SpecHelper.mkLineString (50, 50) (0, 10) []
  , SpecHelper.mkLineString (0, 0) (60, 60) []
  ]

geoResultLine :: Geospatial.GeoLine
geoResultLine = Geospatial.GeoLine (SpecHelper.mkLineString (10, 10.625) (45, 50) [(50, 60)])

geoLinearRingTst1 :: LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS
geoLinearRingTst1 = SpecHelper.mkLinearRing (50,150) (200, 50) (350,150) [(350,300), (250,300), (200,250), (150,350), (100,250), (100,200)]

geoLinearRingTst2 :: LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS
geoLinearRingTst2 = SpecHelper.mkLinearRing (100,150) (100,207) (250,250) [(250,150),(100,150)]

geoBrokenLinearRingTst :: LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS
geoBrokenLinearRingTst = SpecHelper.mkLinearRing (-512,-400) (96,-400) (96,-904) [(-512,-904),(-512,-400)]

geoGiantLinearRingTst :: LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS
geoGiantLinearRingTst = SpecHelper.mkLinearRing (-128,-128) (2176,-128) (2176,2176) [(-128,2176), (-128,-128)]

geoTurningLinearRingTst :: LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS
geoTurningLinearRingTst = SpecHelper.mkLinearRing (125,125) (175,175) (75,225) [(25,175), (125,125)]

geoPolyTst :: Geospatial.GeoPolygon
geoPolyTst = Geospatial.GeoPolygon (Vector.singleton geoLinearRingTst1)

geoPolysTst :: Geospatial.GeoMultiPolygon
geoPolysTst = Geospatial.GeoMultiPolygon (Vector.fromList [
    Vector.singleton geoLinearRingTst1,
    Vector.singleton geoLinearRingTst2
  ])

geoBrokenPolyTst :: Geospatial.GeoPolygon
geoBrokenPolyTst = Geospatial.GeoPolygon (Vector.singleton geoBrokenLinearRingTst)

geoGiantPolyTst :: Geospatial.GeoPolygon
geoGiantPolyTst = Geospatial.GeoPolygon (Vector.singleton geoGiantLinearRingTst)

geoTurningPolyTst :: Geospatial.GeoPolygon
geoTurningPolyTst = Geospatial.GeoPolygon (Vector.singleton geoTurningLinearRingTst)

geoLineFeatureTst :: Geospatial.GeoFeature Aeson.Value
geoLineFeatureTst = Geospatial.GeoFeature Nothing (Geospatial.Line geoLineTst) Aeson.Null Nothing

geoLinesFeatureTst :: Geospatial.GeoFeature Aeson.Value
geoLinesFeatureTst = Geospatial.GeoFeature Nothing (Geospatial.MultiLine geoLinesTst) Aeson.Null Nothing

geoPolygonFeatureTst :: Geospatial.GeoFeature Aeson.Value
geoPolygonFeatureTst = Geospatial.GeoFeature Nothing (Geospatial.Polygon geoPolyTst) Aeson.Null Nothing

geoBrokenPolyFeatureTst :: Geospatial.GeoFeature Aeson.Value
geoBrokenPolyFeatureTst = Geospatial.GeoFeature Nothing (Geospatial.Polygon geoBrokenPolyTst) Aeson.Null Nothing

geoGiantPolyFeatureTst :: Geospatial.GeoFeature Aeson.Value
geoGiantPolyFeatureTst = Geospatial.GeoFeature Nothing (Geospatial.Polygon geoGiantPolyTst) Aeson.Null Nothing

geoTurningPolyFeatureTst :: Geospatial.GeoFeature Aeson.Value
geoTurningPolyFeatureTst = Geospatial.GeoFeature Nothing (Geospatial.Polygon geoTurningPolyTst) Aeson.Null Nothing

geoResultLines :: Geospatial.GeoMultiLine
geoResultLines = Geospatial.GeoMultiLine $ Vector.fromList
  [ SpecHelper.mkLineString (10, 10) (60, 60) []
  , SpecHelper.mkLineString (50, 50) (10, 18) []
  , SpecHelper.mkLineString (10, 10) (10, 10) []
  , SpecHelper.mkLineString (10, 10.625) (45, 50) [(50, 60)]
  , SpecHelper.mkLineString (11, 11) (59, 59) []
  ]

geoResultLineFeatureTst :: Vector.Vector (Geospatial.GeoFeature Aeson.Value)
geoResultLineFeatureTst = Vector.singleton $ Geospatial.GeoFeature Nothing (Geospatial.Line geoResultLine) Aeson.Null Nothing

geoResultLinesFeatureTst :: Vector.Vector (Geospatial.GeoFeature Aeson.Value)
geoResultLinesFeatureTst = Vector.singleton $ Geospatial.GeoFeature Nothing (Geospatial.MultiLine geoResultLines) Aeson.Null Nothing

geoResultLinearRing1 :: LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS
geoResultLinearRing1 = SpecHelper.mkLinearRing (100,200) (100,116.66666666666667) (125.00000000000001,100) [(275,100),(300,116.66666666666667),(300,300),(250,300),(200,250),(175,300),(125,300),(100,250),(100,200)]

geoResultLinearRing2 :: LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS
geoResultLinearRing2 = SpecHelper.mkLinearRing (100,150) (100,207) (250,250) [(250,150),(100,150)]

geoResultGiantLinearRing :: LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS
geoResultGiantLinearRing = SpecHelper.mkLinearRing (-128, -128) (2176, -128) (2176, 2176) [(-128, 2176), (-128, -128)]

geoResultTurningRing :: LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS
geoResultTurningRing = SpecHelper.mkLinearRing (125,125) (175,175) (125,200) [(100,200),(100,137.5),(125,125)]

geoResultPolyFeatureTst :: Vector.Vector (Geospatial.GeoFeature Aeson.Value)
geoResultPolyFeatureTst = Vector.singleton $ Geospatial.GeoFeature Nothing (Geospatial.Polygon (Geospatial.GeoPolygon (Vector.singleton geoResultLinearRing1))) Aeson.Null Nothing

geoResultPolysFeatureTst :: Vector.Vector (Geospatial.GeoFeature Aeson.Value)
geoResultPolysFeatureTst = Vector.singleton $ Geospatial.GeoFeature Nothing (Geospatial.MultiPolygon (Geospatial.GeoMultiPolygon (Vector.fromList [Vector.singleton geoResultLinearRing1, Vector.singleton geoResultLinearRing2]))) Aeson.Null Nothing

geoResultGiantPolyFeatureTst :: Vector.Vector (Geospatial.GeoFeature Aeson.Value)
geoResultGiantPolyFeatureTst = Vector.singleton $ Geospatial.GeoFeature Nothing (Geospatial.Polygon (Geospatial.GeoPolygon (Vector.singleton geoResultGiantLinearRing))) Aeson.Null Nothing

geoResultTurningPolyFeatureTst :: Vector.Vector (Geospatial.GeoFeature Aeson.Value)
geoResultTurningPolyFeatureTst = Vector.singleton $ Geospatial.GeoFeature Nothing (Geospatial.Polygon (Geospatial.GeoPolygon (Vector.singleton geoResultTurningRing))) Aeson.Null Nothing

lineClip :: GeometryGeography.BoundingBox
lineClip = GeometryGeography.BoundingBox 10 10 60 60

polyClip :: GeometryGeography.BoundingBox
polyClip = GeometryGeography.BoundingBox 100 100 300 300

brokenClip :: GeometryGeography.BoundingBox
brokenClip = GeometryGeography.BoundingBox (-128) (-128) 2176 2176

giantClip :: GeometryGeography.BoundingBox
giantClip = GeometryGeography.BoundingBox (-128) (-128) 2176 2176

turningClip :: GeometryGeography.BoundingBox
turningClip = GeometryGeography.BoundingBox 100 100 200 200

spec :: Spec
spec = do
  testClipLine
  testClipPolygon
  testClipPolygonWithInterior
  testManyClipPolygon

testClipLine :: Spec
testClipLine =
  describe "simple line test" $ do
    it "Cohen Sutherland returns clipped line" $ do
      let actual = GeometryClip.clipLineCs lineClip geoLineTst geoLineFeatureTst Vector.empty
      actual `shouldBe` geoResultLineFeatureTst
    it "Cohen Sutherland returns clipped lines" $ do
      let actual = GeometryClip.clipLinesCs lineClip geoLinesTst geoLinesFeatureTst Vector.empty
      actual `shouldBe` geoResultLinesFeatureTst
    it "Liang Barsky returns clipped line" $ do
      let actual = GeometryClip.clipLinesLb lineClipPts linesTst
      actual `shouldBe` resultLines
    it "QuickClip returns clipped line" $ do
      let actual = GeometryClip.clipLineQc lineClip geoLineTst geoLineFeatureTst Vector.empty
      actual `shouldBe` geoResultLineFeatureTst
    it "QuickClip returns clipped lines" $ do
      let actual = GeometryClip.clipLinesQc lineClip geoLinesTst geoLinesFeatureTst Vector.empty
      actual `shouldBe` geoResultLinesFeatureTst
    it "Nicholl-Lee-Nicholl returns clipped line" $ do
      let actual = GeometryClip.clipLinesNLN lineClipPts linesTst
      actual `shouldBe` resultLines

testClipPolygon :: Spec
testClipPolygon =
  describe "simple polygon test" $ do
    it "Simple - Returns clipped polygon" $
      GeometryClip.clipPolygon polyClip geoPolyTst geoPolygonFeatureTst Vector.empty `shouldBe` geoResultPolyFeatureTst
    it "Simple - Returns clipped multipolygon" $
      GeometryClip.clipPolygons polyClip geoPolysTst geoPolygonFeatureTst Vector.empty `shouldBe` geoResultPolysFeatureTst
    it "Simple - Negative polygon" $
      GeometryClip.clipPolygon brokenClip geoBrokenPolyTst geoBrokenPolyFeatureTst Vector.empty `shouldBe` Vector.empty
    it "Simple - Maximum polygon" $
      GeometryClip.clipPolygon giantClip geoGiantPolyTst geoGiantPolyFeatureTst Vector.empty `shouldBe` geoResultGiantPolyFeatureTst
    it "Simple - Turning point test" $
      GeometryClip.clipPolygon myClipPts turningPointTestPoly `shouldBe` Just turningPointTestClippedPoly
    -- it "NLN - Returns clipped polygon" $
      GeometryClip.clipPolygon turningClip geoTurningPolyTst geoTurningPolyFeatureTst Vector.empty `shouldBe` geoResultTurningPolyFeatureTst
    -- it "QC - Returns clipped polygon" $
    --   GeometryClip.clipPolygonNLN polyClipPts poly `shouldBe` Just resultPoly
    it "NLN - Negative polygon" $ do
      let actual = GeometryClip.clipPolygonNLN brokenClipPts brokenPoly
      actual `shouldBe` Nothing
    it "NLN - Maximum polygon" $ do
      let actual = GeometryClip.clipPolygonNLN giantClipPts giantPoly
          resultPts = [(-128,-128),(2176,-128),(2176,2176),(-128,2176),(-128,-128)]
          result = VectorTile.Polygon (SpecHelper.tupleToPts resultPts) mempty
      actual `shouldBe` Just result
    -- it "NLN - Turning point test" $
    --   GeometryClip.clipPolygonNLN myClipPts turningPointTestPoly `shouldBe` Just turningPointTestClippedPoly


testClipPolygonWithInterior :: Spec
testClipPolygonWithInterior =
  describe "simple polygon with inner test" $
    it "Returns clipped polygon and inner polygon" $
      GeometryClip.clipPolygon polyClipPts polyWithInner `shouldBe` Just resultPolyWithInner


manyClipPts :: GeometryGeography.BoundingBoxPts
manyClipPts = GeometryGeography.BoundingBoxPts (VectorTile.Point 100 100) (VectorTile.Point 200 200)
    
poly1 :: VectorTile.Polygon
poly1 = VectorTile.Polygon (SpecHelper.tupleToPts a) mempty
  where
    a = [(125,125),(175,175),(75,225),(25,175),(125,125)]

result1 :: VectorTile.Polygon
result1 = VectorTile.Polygon (SpecHelper.tupleToPts a) mempty
  where
    a = [(125,125),(175,175),(124,200),(100,200),(100,137),(125,125)]

result1b :: VectorTile.Polygon
result1b = VectorTile.Polygon (SpecHelper.tupleToPts a) mempty
  where
    a = [(125,125),(175,175),(125,200),(100,200),(100,137),(125,125)]
  
testManyClipPolygon :: Spec
testManyClipPolygon = 
  describe "many polygon test" $ do
    it "Simple - polygon 1" $
      GeometryClip.clipPolygon manyClipPts poly1 `shouldBe` Just result1
    it "NLN - polygon 1" $
      GeometryClip.clipPolygonNLN manyClipPts poly1 `shouldBe` Just result1b
