{-# LANGUAGE OverloadedStrings #-}

module Data.Geometry.ClipSpec where

import qualified Data.Sequence             as DS
import qualified Data.Vector.Unboxed       as DVU
import qualified Geography.VectorTile      as VG
import           Test.Hspec                (Spec, describe, it, shouldBe)

import           Data.Geometry.Clip        as DGC
import           Data.Geometry.Types.Types as DGT

brokenPoly :: VG.Polygon
brokenPoly = VG.Polygon (DVU.fromList brokenPolyPts) mempty

giantPoly :: VG.Polygon
giantPoly = VG.Polygon (DVU.fromList giantPolyPts) mempty

polyPts :: [(Int, Int)]
polyPts = [ (50,150), (200, 50), (350,150), (350,300), (250,300),
             (200,250), (150,350), (100,250), (100,200)]

innerPolyPts :: [(Int, Int)]
innerPolyPts = [(75,200),(250,250),(250,150),(75,150)]

poly :: VG.Polygon
poly = VG.Polygon (DVU.fromList polyPts) mempty

resultPolyPts :: [(Int, Int)]
resultPolyPts = [(100,250),(100,116),(124,100),(275,100),(300,116),(300,300),(250,300),(200,250),(175,300),(125,300),(100,250)]

innerPolyResultPts :: [(Int, Int)]
innerPolyResultPts = [(100,150),(100,207),(250,250),(250,150),(100,150)]

resultPoly :: VG.Polygon
resultPoly = VG.Polygon (DVU.fromList resultPolyPts) mempty

polyWithInner :: VG.Polygon
polyWithInner = VG.Polygon (DVU.fromList polyPts) (DS.fromList [VG.Polygon (DVU.fromList innerPolyPts) mempty])

resultPolyWithInner :: VG.Polygon
resultPolyWithInner = VG.Polygon (DVU.fromList resultPolyPts) (DS.fromList [VG.Polygon (DVU.fromList innerPolyResultPts) mempty])

brokenPolyPts :: [(Int, Int)]
brokenPolyPts = [(-512,-400),(96,-400),(96,-904),(-512,-904),(-512,-400)]

giantPolyPts :: [(Int, Int)]
giantPolyPts = [(2176,-128),(2176,2176),(-128,2176),(-128,-128)]

polyClipPts :: BoundingBoxPts
polyClipPts = BoundingBoxPts (100, 100) (300, 300)

brokenClipPts :: BoundingBoxPts
brokenClipPts = BoundingBoxPts (-128,-128) (2176,2176)

giantClipPts :: BoundingBoxPts
giantClipPts = BoundingBoxPts (-128,-128) (2176,2176)

linesTst :: DS.Seq VG.LineString
linesTst = DS.fromList
  [ VG.LineString (DVU.fromList [(11, 11), (59, 59)])
  , VG.LineString (DVU.fromList [(0, 0), (0, 100)])
  , VG.LineString (DVU.fromList [(5, 5), (45, 50), (90, 140)])
  , VG.LineString (DVU.fromList [(0, 0), (60, 60)])]

lineClipPts :: BoundingBoxPts
lineClipPts = BoundingBoxPts (10,10) (60,60)

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
          resultLines = DS.fromList
            [ VG.LineString (DVU.fromList [(10, 10), (60, 60)])
            , VG.LineString (DVU.fromList [(10, 11), (45, 50), (50, 60)])
            , VG.LineString (DVU.fromList [(11, 11), (59, 59)])
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
          result = VG.Polygon (DVU.fromList resultPts) mempty
      actual `shouldBe` Just result

testClipPolygonWithInterior :: Spec
testClipPolygonWithInterior =
  describe "simple polygon with inner test" $
    it "Returns clipped polygon and inner polygon" $
      clipPolygon polyClipPts polyWithInner `shouldBe` Just resultPolyWithInner
