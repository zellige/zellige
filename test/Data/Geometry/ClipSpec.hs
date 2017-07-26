{-# LANGUAGE OverloadedStrings #-}

module Data.Geometry.ClipSpec where

import qualified Data.Vector                   as DV
import qualified Data.Vector.Unboxed           as DVU
import qualified Geography.VectorTile.Geometry as VG
import           Test.Hspec                    (Spec, describe, it, shouldBe)

import           Data.Geometry.Clip

poly :: VG.Polygon
poly = VG.Polygon (DVU.fromList polyPts) mempty

brokenPoly :: VG.Polygon
brokenPoly = VG.Polygon (DVU.fromList brokenPolyPts) mempty

giantPoly :: VG.Polygon
giantPoly = VG.Polygon (DVU.fromList giantPolyPts) mempty

polyPts :: [(Int, Int)]
polyPts = [ (50,150), (200, 50), (350,150), (350,300), (250,300),
             (200,250), (150,350), (100,250), (100,200)]

brokenPolyPts :: [(Int, Int)]
brokenPolyPts = [(-512,-400),(96,-400),(96,-904),(-512,-904),(-512,-400)]

giantPolyPts :: [(Int, Int)]
giantPolyPts = [(2176,-128),(2176,2176),(-128,2176),(-128,-128)]

polyClipPts :: ((Int, Int), (Int, Int))
polyClipPts = ((100, 100), (300, 300))

brokenClipPts :: ((Int, Int), (Int, Int))
brokenClipPts = ((-128,-128),(2176,2176))

giantClipPts :: ((Int, Int), (Int, Int))
giantClipPts = ((-128,-128),(2176,2176))

linesTst :: DV.Vector VG.LineString
linesTst = DV.fromList
  [ VG.LineString (DVU.fromList [(11, 11), (59, 59)])
  , VG.LineString (DVU.fromList [(0, 0), (0, 100)])
  , VG.LineString (DVU.fromList [(5, 5), (45, 50), (90, 140)])
  , VG.LineString (DVU.fromList [(0, 0), (60, 60)])]

lineClipPts :: ((Int, Int), (Int, Int))
lineClipPts = ((10,10),(60,60))

spec :: Spec
spec = do
  testClipLine
  testClipPolygon

testClipLine :: Spec
testClipLine =
  describe "simple line test" $
    it "Returns clipped line" $ do
      let actual = clipLines lineClipPts linesTst
          resultLines = DV.fromList
            [ VG.LineString (DVU.fromList [(11, 11), (59, 59)])
            , VG.LineString (DVU.fromList [])
            , VG.LineString (DVU.fromList [(10, 11), (45, 50), (50, 60)])
            , VG.LineString (DVU.fromList [(10, 10), (60, 60)])
            ]
      actual `shouldBe` resultLines

testClipPolygon :: Spec
testClipPolygon =
  describe "simple polygon test" $ do
    it "Returns clipped polygon" $ do
      let actual = clipPolygon polyClipPts poly
          resultPts = [(100,250),(100,116),(124,100),(275,100),(300,116),(300,300),(250,300),(200,250),(175,300),(125,300),(100,250)]
          result = VG.Polygon (DVU.fromList resultPts) mempty
      actual `shouldBe` result
    it "Negative polygon" $ do
      let actual = clipPolygon brokenClipPts brokenPoly
          resultPts = []
          result = VG.Polygon (DVU.fromList resultPts) mempty
      actual `shouldBe` result
    it "Maximum polygon" $ do
      let actual = clipPolygon giantClipPts giantPoly
          resultPts = [(-128,-128),(2176,-128),(2176,2176),(-128,2176),(-128,-128)]
          result = VG.Polygon (DVU.fromList resultPts) mempty
      actual `shouldBe` result

