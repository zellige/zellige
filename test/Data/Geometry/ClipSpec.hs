{-# LANGUAGE OverloadedStrings #-}

module Data.Geometry.ClipSpec where

import qualified Control.Monad                       as ControlMonad
import qualified Data.Aeson                          as Aeson
import qualified Data.Geometry.VectorTile.VectorTile as VectorTile
import qualified Data.Geospatial                     as Geospatial
import qualified Data.LinearRing                     as LinearRing
import qualified Data.Sequence                       as Sequence
import           Test.Hspec                          (Spec, describe, it,
                                                      shouldBe)

import qualified Data.Geometry.Clip                  as GeometryClip
import qualified Data.Geometry.Clip.Internal.Line    as InternalLine
import qualified Data.Geometry.Types.Geography       as GeometryGeography

import qualified Data.SpecHelper                     as SpecHelper

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

linesTst :: Sequence.Seq VectorTile.LineString
linesTst = Sequence.fromList
  [ VectorTile.LineString (SpecHelper.tupleToPts [(11, 11), (59, 59)])
  , VectorTile.LineString (SpecHelper.tupleToPts [(0, 0), (0, 100)])
  , VectorTile.LineString (SpecHelper.tupleToPts [(5, 5), (45, 50), (90, 140)])
  , VectorTile.LineString (SpecHelper.tupleToPts [(0, 0), (10, 10)])
  , VectorTile.LineString (SpecHelper.tupleToPts [(50, 50), (0, 10)])
  , VectorTile.LineString (SpecHelper.tupleToPts [(0, 0), (60, 60)])]

resultLines :: Sequence.Seq VectorTile.LineString
resultLines = Sequence.fromList
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
resultPolyWithInner = VectorTile.Polygon (SpecHelper.tupleToPts resultPolyPts) (Sequence.fromList [VectorTile.Polygon (SpecHelper.tupleToPts innerPolyResultPts) mempty])

geoLineTst :: Geospatial.GeoLine
geoLineTst = Geospatial.GeoLine (SpecHelper.mkLineString (5, 5) (45, 50) [(90, 140)])

geoLinesTst :: Geospatial.GeoMultiLine
geoLinesTst = Geospatial.GeoMultiLine $ Sequence.fromList
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
geoLinearRingTst1 = SpecHelper.mkLinearRing (50, 50) (50,150) (200, 50) [(350, 50), (350,150), (350, 350), (350,300), (250,300), (200,250), (50, 350), (150,350), (100,250), (100,200)]

geoLinearRingTst2 :: LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS
geoLinearRingTst2 = SpecHelper.mkLinearRing (100,150) (100,207) (250,250) [(250,150),(100,150)]

geoBrokenLinearRingTst :: LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS
geoBrokenLinearRingTst = SpecHelper.mkLinearRing (-512,-400) (96,-400) (96,-904) [(-512,-904),(-512,-400)]

geoGiantLinearRingTst :: LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS
geoGiantLinearRingTst = SpecHelper.mkLinearRing (-128,-128) (2176,-128) (2176,2176) [(-128,2176), (-128,-128)]

geoTurningLinearRingTst :: LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS
geoTurningLinearRingTst = SpecHelper.mkLinearRing (125,125) (175,175) (75,225) [(25,175), (125,125)]

geoPolyTst :: Geospatial.GeoPolygon
geoPolyTst = Geospatial.GeoPolygon (Sequence.singleton geoLinearRingTst1)

geoPolysTst :: Geospatial.GeoMultiPolygon
geoPolysTst = Geospatial.GeoMultiPolygon (Sequence.fromList [
    Sequence.singleton geoLinearRingTst1,
    Sequence.singleton geoLinearRingTst2
  ])

geoBrokenPolyTst :: Geospatial.GeoPolygon
geoBrokenPolyTst = Geospatial.GeoPolygon (Sequence.singleton geoBrokenLinearRingTst)

geoGiantPolyTst :: Geospatial.GeoPolygon
geoGiantPolyTst = Geospatial.GeoPolygon (Sequence.singleton geoGiantLinearRingTst)

geoTurningPolyTst :: Geospatial.GeoPolygon
geoTurningPolyTst = Geospatial.GeoPolygon (Sequence.singleton geoTurningLinearRingTst)

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
geoResultLines = Geospatial.GeoMultiLine $ Sequence.fromList
  [ SpecHelper.mkLineString (10, 10) (60, 60) []
  , SpecHelper.mkLineString (50, 50) (10, 18) []
  , SpecHelper.mkLineString (10, 10) (10, 10) []
  , SpecHelper.mkLineString (10, 10.625) (45, 50) [(50, 60)]
  , SpecHelper.mkLineString (11, 11) (59, 59) []
  ]

geoResultLineFeatureTst :: Sequence.Seq (Geospatial.GeoFeature Aeson.Value)
geoResultLineFeatureTst = Sequence.singleton $ Geospatial.GeoFeature Nothing (Geospatial.Line geoResultLine) Aeson.Null Nothing

geoResultLinesFeatureTst :: Sequence.Seq (Geospatial.GeoFeature Aeson.Value)
geoResultLinesFeatureTst = Sequence.singleton $ Geospatial.GeoFeature Nothing (Geospatial.MultiLine geoResultLines) Aeson.Null Nothing

geoResultLinearRing1 :: LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS
geoResultLinearRing1 = SpecHelper.mkLinearRing (100,200) (100,116.66666666666667) (125.00000000000001,100) [(300,100),(300,300),(250,300),(200,250),(124.99999999999999,300),(125,300),(100,250),(100,200)]

offGeoResultLinearRing1 :: LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS
offGeoResultLinearRing1 = SpecHelper.mkLinearRing (100,200) (100,116.66666666666666) (125.0,100) [(300,100),(300,300),(250,300),(200,250),(125,300),(125,300),(100,250),(100,200)]

geoResultLinearRing2 :: LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS
geoResultLinearRing2 = SpecHelper.mkLinearRing (100,150) (100,207) (250,250) [(250,150),(100,150)]

geoResultGiantLinearRing :: LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS
geoResultGiantLinearRing = SpecHelper.mkLinearRing (-128, -128) (2176, -128) (2176, 2176) [(-128, 2176), (-128, -128)]

geoResultTurningRing :: LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS
geoResultTurningRing = SpecHelper.mkLinearRing (125,125) (175,175) (125,200) [(100,200),(100,137.5),(125,125)]

geoResultPolyFeatureTst :: Sequence.Seq (Geospatial.GeoFeature Aeson.Value)
geoResultPolyFeatureTst = Sequence.singleton $ Geospatial.GeoFeature Nothing (Geospatial.Polygon (Geospatial.GeoPolygon (Sequence.singleton geoResultLinearRing1))) Aeson.Null Nothing

offGeoResultPolyFeatureTst :: Sequence.Seq (Geospatial.GeoFeature Aeson.Value)
offGeoResultPolyFeatureTst = Sequence.singleton $ Geospatial.GeoFeature Nothing (Geospatial.Polygon (Geospatial.GeoPolygon (Sequence.singleton offGeoResultLinearRing1))) Aeson.Null Nothing

geoResultPolysFeatureTst :: Sequence.Seq (Geospatial.GeoFeature Aeson.Value)
geoResultPolysFeatureTst = Sequence.singleton $ Geospatial.GeoFeature Nothing (Geospatial.MultiPolygon (Geospatial.GeoMultiPolygon (Sequence.fromList [Sequence.singleton geoResultLinearRing1, Sequence.singleton geoResultLinearRing2]))) Aeson.Null Nothing

offGeoResultPolysFeatureTst :: Sequence.Seq (Geospatial.GeoFeature Aeson.Value)
offGeoResultPolysFeatureTst = Sequence.singleton $ Geospatial.GeoFeature Nothing (Geospatial.MultiPolygon (Geospatial.GeoMultiPolygon (Sequence.fromList [Sequence.singleton offGeoResultLinearRing1, Sequence.singleton geoResultLinearRing2]))) Aeson.Null Nothing

geoResultGiantPolyFeatureTst :: Sequence.Seq (Geospatial.GeoFeature Aeson.Value)
geoResultGiantPolyFeatureTst = Sequence.singleton $ Geospatial.GeoFeature Nothing (Geospatial.Polygon (Geospatial.GeoPolygon (Sequence.singleton geoResultGiantLinearRing))) Aeson.Null Nothing

geoResultTurningPolyFeatureTst :: Sequence.Seq (Geospatial.GeoFeature Aeson.Value)
geoResultTurningPolyFeatureTst = Sequence.singleton $ Geospatial.GeoFeature Nothing (Geospatial.Polygon (Geospatial.GeoPolygon (Sequence.singleton geoResultTurningRing))) Aeson.Null Nothing

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
  testLineHelper
  testClipLine
  testClipPolygon
  -- testClipPolygonWithInterior
  -- testManyClipPolygon

testLineHelper :: Spec
testLineHelper =
  describe "segmentToLine" $ do
    it "Simple test" $ do
      let inputPts = Sequence.fromList ([1,2,2,7,7,10,10,11] :: [Int])
          expectedPts = Sequence.fromList ([1,2,7,10,11] :: [Int])
      expectedPts `shouldBe` InternalLine.segmentToLine inputPts
    it "Empty Test" $
      Sequence.empty `shouldBe` InternalLine.segmentToLine (Sequence.empty :: Sequence.Seq Int)
    it "Single element test" $
      Sequence.empty `shouldBe` InternalLine.segmentToLine (Sequence.fromList ([1] :: [Int]))

lineClippingAlgorithms :: [GeometryGeography.BoundingBox -> Geospatial.GeoLine -> Geospatial.GeoFeature Aeson.Value -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value) -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value)]
lineClippingAlgorithms =
    [GeometryClip.clipLineCs, GeometryClip.clipLineLb, GeometryClip.clipLineQc, GeometryClip.clipLineNLN]

multilineClippingAlgorithms :: [GeometryGeography.BoundingBox -> Geospatial.GeoMultiLine -> Geospatial.GeoFeature Aeson.Value -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value) -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value)]
multilineClippingAlgorithms =
    [GeometryClip.clipLinesCs, GeometryClip.clipLinesLb, GeometryClip.clipLinesQc, GeometryClip.clipLinesNLN]

testClipLine :: Spec
testClipLine =
  describe "simple line test" $ do
    it "Algorithms returns clipped line" $ do
      let actual f = f lineClip geoLineTst geoLineFeatureTst Sequence.empty
      ControlMonad.forM_ lineClippingAlgorithms (\x -> actual x `shouldBe` geoResultLineFeatureTst)
    it "Algorithms returns clipped line" $ do
      let actual f = f lineClip geoLinesTst geoLineFeatureTst Sequence.empty
      ControlMonad.forM_ multilineClippingAlgorithms (\x -> actual x `shouldBe` geoResultLinesFeatureTst)

polgonClippingAlgorithms :: [GeometryGeography.BoundingBox -> Geospatial.GeoPolygon -> Geospatial.GeoFeature Aeson.Value -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value) -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value)]
polgonClippingAlgorithms =
    [GeometryClip.clipPolygonSh]

multipolgonClippingAlgorithms :: [GeometryGeography.BoundingBox -> Geospatial.GeoMultiPolygon -> Geospatial.GeoFeature Aeson.Value -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value) -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value)]
multipolgonClippingAlgorithms =
    [GeometryClip.clipPolygonsSh]

testClipPolygon :: Spec
testClipPolygon =
  describe "simple polygon test" $ do
    it "Simple - Returns clipped polygon" $
      ControlMonad.forM_ polgonClippingAlgorithms (\x -> x polyClip geoPolyTst geoPolygonFeatureTst Sequence.empty `shouldBe` geoResultPolyFeatureTst)
    it "Simple - Returns clipped multipolygon" $
      ControlMonad.forM_ multipolgonClippingAlgorithms (\x -> x polyClip geoPolysTst geoPolygonFeatureTst Sequence.empty `shouldBe` geoResultPolysFeatureTst)
    it "Simple - Negative polygon" $
      ControlMonad.forM_ polgonClippingAlgorithms (\x -> x brokenClip geoBrokenPolyTst geoBrokenPolyFeatureTst Sequence.empty `shouldBe` Sequence.empty)
    it "Simple - Maximum polygon" $
      ControlMonad.forM_ polgonClippingAlgorithms (\x -> x giantClip geoGiantPolyTst geoGiantPolyFeatureTst Sequence.empty `shouldBe` geoResultGiantPolyFeatureTst)
    it "Simple - Turning point test" $
      ControlMonad.forM_ polgonClippingAlgorithms (\x -> x turningClip geoTurningPolyTst geoTurningPolyFeatureTst Sequence.empty `shouldBe` geoResultTurningPolyFeatureTst)
