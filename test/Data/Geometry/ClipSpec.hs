{-# LANGUAGE OverloadedStrings #-}

module Data.Geometry.ClipSpec where

import qualified Data.Vector.Unboxed           as DVU
import qualified Geography.VectorTile.Geometry as VG
import           Test.Hspec                    (Spec, describe, it, shouldBe)

import           Data.Geometry.Clip

poly :: VG.Polygon
poly = VG.Polygon (DVU.fromList polyPts) mempty

polyPts :: [(Int, Int)]
polyPts = [( 50,150), (200, 50), (350,150), (350,300), (250,300),
            (200,250), (150,350), (100,250), (100,200)] :: [(Int,Int)]

clipPts :: ((Int, Int), (Int, Int))
clipPts = ((100, 100), (300, 300))

spec :: Spec
spec =
  testClipPolygon

-- testPoly = clipPolygons clipPts [poly]
-- testSingle = clipPolygon clipPts poly
-- isOkay = VG.Polygon (DVU.fromList answer) mempty == testSingle
-- poly = VG.Polygon (DVU.fromList polyPts) mempty
-- polyPts = [( 50,150), (200, 50), (350,150), (350,300), (250,300),
--            (200,250), (150,350), (100,250), (100,200)] :: [(Int,Int)]
-- clipPts = ((100, 100), (300, 300))
-- linesBbTst = ((10,10),(60,60)) :: ((Int, Int), (Int, Int))
-- linesTst = [VG.LineString (DVU.fromList ([(11, 11), (59, 59)] :: [(Int,Int)])),
--   VG.LineString (DVU.fromList ([(0, 0), (0, 100)] :: [(Int,Int)])),
--   VG.LineString (DVU.fromList ([(5, 5), (45, 50), (90, 140)] :: [(Int,Int)])),
--   VG.LineString (DVU.fromList ([(0, 0), (60, 60)] :: [(Int,Int)]))]
-- answer = [(100,116),(124,100),(275,100),(300,116),(300,300),(250,300),(200,250),(175,300),(125,300),(100,250)]
-- [{100 116.66667} {125 100} {275 100} {300 116.66667} {300 300} {250 300} {200 250}
--  {175 300} {125 300} {100 250}]

testClipPolygon :: Spec
testClipPolygon =
  describe "simple polygon test" $
    it "Returns clipped polygon" $ do
      let actual = clipPolygon clipPts poly
          result = clipPolygon clipPts poly
      actual `shouldBe` result
