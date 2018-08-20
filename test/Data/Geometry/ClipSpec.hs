{-# LANGUAGE OverloadedStrings #-}

module Data.Geometry.ClipSpec where

import qualified Data.Vector                   as Vector
import qualified Geography.VectorTile          as VectorTile
import           Test.Hspec                    (Spec, describe, it, shouldBe)

import           Data.Geometry.Clip            as GeometryClip
import           Data.Geometry.Types.Geography as GeometryGeography

import           Data.Geometry.SpecHelper

brokenPoly :: VectorTile.Polygon
brokenPoly = VectorTile.Polygon (tupleToPts brokenPolyPts) mempty

giantPoly :: VectorTile.Polygon
giantPoly = VectorTile.Polygon (tupleToPts giantPolyPts) mempty

polyPts :: [(Int, Int)]
polyPts = [ (50,150), (200, 50), (350,150), (350,300), (250,300),
             (200,250), (150,350), (100,250), (100,200)]

innerPolyPts :: [(Int, Int)]
innerPolyPts = [(75,200),(250,250),(250,150),(75,150)]

poly :: VectorTile.Polygon
poly = VectorTile.Polygon (tupleToPts polyPts) mempty

resultPolyPts :: [(Int, Int)]
resultPolyPts = [(100,250),(100,116),(124,100),(275,100),(300,116),(300,300),(250,300),(200,250),(175,300),(125,300),(100,250)]

innerPolyResultPts :: [(Int, Int)]
innerPolyResultPts = [(100,150),(100,207),(250,250),(250,150),(100,150)]

resultPoly :: VectorTile.Polygon
resultPoly = VectorTile.Polygon (tupleToPts resultPolyPts) mempty

polyWithInner :: VectorTile.Polygon
polyWithInner = VectorTile.Polygon (tupleToPts polyPts) (Vector.fromList [VectorTile.Polygon (tupleToPts innerPolyPts) mempty])

resultPolyWithInner :: VectorTile.Polygon
resultPolyWithInner = VectorTile.Polygon (tupleToPts resultPolyPts) (Vector.fromList [VectorTile.Polygon (tupleToPts innerPolyResultPts) mempty])

brokenPolyPts :: [(Int, Int)]
brokenPolyPts = [(-512,-400),(96,-400),(96,-904),(-512,-904),(-512,-400)]

giantPolyPts :: [(Int, Int)]
giantPolyPts = [(2176,-128),(2176,2176),(-128,2176),(-128,-128)]

polyClipPts :: BoundingBoxPts
polyClipPts = BoundingBoxPts (VectorTile.Point 100 100) (VectorTile.Point 300 300)

brokenClipPts :: BoundingBoxPts
brokenClipPts = BoundingBoxPts (VectorTile.Point (-128) (-128)) (VectorTile.Point 2176 2176)

giantClipPts :: BoundingBoxPts
giantClipPts = BoundingBoxPts (VectorTile.Point (-128) (-128)) (VectorTile.Point 2176 2176)

linesTst :: Vector.Vector VectorTile.LineString
linesTst = Vector.fromList
  [ VectorTile.LineString (tupleToPts [(11, 11), (59, 59)])
  , VectorTile.LineString (tupleToPts [(0, 0), (0, 100)])
  , VectorTile.LineString (tupleToPts [(5, 5), (45, 50), (90, 140)])
  , VectorTile.LineString (tupleToPts [(0, 0), (60, 60)])]

lineClipPts :: BoundingBoxPts
lineClipPts = BoundingBoxPts (VectorTile.Point 10 10) (VectorTile.Point 60 60)

spec :: Spec
spec = do
  testClipLine
  testClipPolygon
  testClipPolygonWithInterior

testClipLine :: Spec
testClipLine =
  describe "simple line test" $
    it "Returns clipped line" $ do
      let actual = clipLines lineClipPts linesTst
          resultLines = Vector.fromList
            [ VectorTile.LineString (tupleToPts [(10, 10), (60, 60)])
            , VectorTile.LineString (tupleToPts [(10, 11), (45, 50), (50, 60)])
            , VectorTile.LineString (tupleToPts [(11, 11), (59, 59)])
            ]
      actual `shouldBe` resultLines

testClipPolygon :: Spec
testClipPolygon =
  describe "simple polygon test" $ do
    it "Returns clipped polygon" $
      clipPolygon polyClipPts poly `shouldBe` Just resultPoly
    it "Negative polygon" $ do
      let actual = clipPolygon brokenClipPts brokenPoly
      actual `shouldBe` Nothing
    it "Maximum polygon" $ do
      let actual = clipPolygon giantClipPts giantPoly
          resultPts = [(-128,-128),(2176,-128),(2176,2176),(-128,2176),(-128,-128)]
          result = VectorTile.Polygon (tupleToPts resultPts) mempty
      actual `shouldBe` Just result

testClipPolygonWithInterior :: Spec
testClipPolygonWithInterior =
  describe "simple polygon with inner test" $
    it "Returns clipped polygon and inner polygon" $
      clipPolygon polyClipPts polyWithInner `shouldBe` Just resultPolyWithInner
