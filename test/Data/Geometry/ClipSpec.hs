{-# LANGUAGE OverloadedStrings #-}

module Data.Geometry.ClipSpec where

import qualified Data.Vector               as Vector
import qualified Geography.VectorTile      as VG
import           Test.Hspec                (Spec, describe, it, shouldBe)

import           Data.Geometry.Clip        as DGC
import           Data.Geometry.Types.Types as DGT

import           Data.Geometry.SpecHelper

brokenPoly :: VG.Polygon
brokenPoly = VG.Polygon (tupleToPts brokenPolyPts) mempty

giantPoly :: VG.Polygon
giantPoly = VG.Polygon (tupleToPts giantPolyPts) mempty

polyPts :: [(Int, Int)]
polyPts = [ (50,150), (200, 50), (350,150), (350,300), (250,300),
             (200,250), (150,350), (100,250), (100,200)]

innerPolyPts :: [(Int, Int)]
innerPolyPts = [(75,200),(250,250),(250,150),(75,150)]

poly :: VG.Polygon
poly = VG.Polygon (tupleToPts polyPts) mempty

resultPolyPts :: [(Int, Int)]
resultPolyPts = [(100,250),(100,116),(124,100),(275,100),(300,116),(300,300),(250,300),(200,250),(175,300),(125,300),(100,250)]

innerPolyResultPts :: [(Int, Int)]
innerPolyResultPts = [(100,150),(100,207),(250,250),(250,150),(100,150)]

resultPoly :: VG.Polygon
resultPoly = VG.Polygon (tupleToPts resultPolyPts) mempty

polyWithInner :: VG.Polygon
polyWithInner = VG.Polygon (tupleToPts polyPts) (Vector.fromList [VG.Polygon (tupleToPts innerPolyPts) mempty])

resultPolyWithInner :: VG.Polygon
resultPolyWithInner = VG.Polygon (tupleToPts resultPolyPts) (Vector.fromList [VG.Polygon (tupleToPts innerPolyResultPts) mempty])

brokenPolyPts :: [(Int, Int)]
brokenPolyPts = [(-512,-400),(96,-400),(96,-904),(-512,-904),(-512,-400)]

giantPolyPts :: [(Int, Int)]
giantPolyPts = [(2176,-128),(2176,2176),(-128,2176),(-128,-128)]

polyClipPts :: BoundingBoxPts
polyClipPts = BoundingBoxPts (VG.Point 100 100) (VG.Point 300 300)

brokenClipPts :: BoundingBoxPts
brokenClipPts = BoundingBoxPts (VG.Point (-128) (-128)) (VG.Point 2176 2176)

giantClipPts :: BoundingBoxPts
giantClipPts = BoundingBoxPts (VG.Point (-128) (-128)) (VG.Point 2176 2176)

linesTst :: Vector.Vector VG.LineString
linesTst = Vector.fromList
  [ VG.LineString (tupleToPts [(11, 11), (59, 59)])
  , VG.LineString (tupleToPts [(0, 0), (0, 100)])
  , VG.LineString (tupleToPts [(5, 5), (45, 50), (90, 140)])
  , VG.LineString (tupleToPts [(0, 0), (60, 60)])]

lineClipPts :: BoundingBoxPts
lineClipPts = BoundingBoxPts (VG.Point 10 10) (VG.Point 60 60)

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
            [ VG.LineString (tupleToPts [(10, 10), (60, 60)])
            , VG.LineString (tupleToPts [(10, 11), (45, 50), (50, 60)])
            , VG.LineString (tupleToPts [(11, 11), (59, 59)])
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
          result = VG.Polygon (tupleToPts resultPts) mempty
      actual `shouldBe` Just result

testClipPolygonWithInterior :: Spec
testClipPolygonWithInterior =
  describe "simple polygon with inner test" $
    it "Returns clipped polygon and inner polygon" $
      clipPolygon polyClipPts polyWithInner `shouldBe` Just resultPolyWithInner
