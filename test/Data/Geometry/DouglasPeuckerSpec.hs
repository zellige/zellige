{-# LANGUAGE OverloadedStrings #-}

module Data.Geometry.DouglasPeuckerSpec where

import qualified Data.Geospatial                       as Geospatial
import qualified Data.Sequence                         as Sequence
import           Test.Hspec                            (Spec, describe, it,
                                                        shouldBe)

import           Data.Geometry.Simplify.DouglasPeucker as SimplifyDouglasPuecker

import           Data.SpecHelper                       as SpecHelper

linePts1 :: Sequence.Seq Geospatial.PointXY
linePts1 = SpecHelper.listToSequenceGeo [(0,0),(10,1),(20,-1),(30,50),(40,60),(50,70),(60,81),(70,90),(80,90),(90,90)]

linePts2 :: Sequence.Seq Geospatial.PointXY
linePts2 = SpecHelper.listToSequenceGeo [(24,173),(26,170),(24,166),(27,162),(37,161),(45,157),(48,152),(46,143),(40,140),(34,137),(26,134),(24,130),(24,125),(28,121),(36,118),(46,117),(63,121),(76,125),(82,120),(86,111),(88,103),(90,91),(95,87),(107,89),(107,104),(106,117),(109,129),(119,131),(131,131),(139,134),(138,143),(131,152),(119,154),(111,149),(105,143),(91,139),(80,142),(81,152),(76,163),(67,161),(59,149),(63,138)]

spec :: Spec
spec = do
  testKnownPts
  testAnotherPts

testKnownPts :: Spec
testKnownPts =
  describe "base case" $
    it "Returns simplified line epsilon 1" $ do
      let actual = douglasPeucker 1.0 linePts1
          resultPts = SpecHelper.listToSequenceGeo [(0,0),(10,1),(20,-1),(30,50),(70,90),(90,90)]
      actual `shouldBe` resultPts

testAnotherPts :: Spec
testAnotherPts =
  describe "more complicated" $ do
    it "Returns simplified line epsilon 10" $ do
        let actual = douglasPeucker 10.0 linePts2
            resultPts = SpecHelper.listToSequenceGeo [(24,173),(48,152),(24,125),(76,125),(95,87),(107,89),(109,129),(139,134),(119,154),(80,142),(76,163),(63,138)]
        actual `shouldBe` resultPts
    it "Returns simplified line epsilon 20" $ do
        let actual = douglasPeucker 20.0 linePts2
            resultPts = SpecHelper.listToSequenceGeo [(24,173),(48,152),(24,125),(76,125),(95,87),(139,134),(76,163),(63,138)]
        actual `shouldBe` resultPts
