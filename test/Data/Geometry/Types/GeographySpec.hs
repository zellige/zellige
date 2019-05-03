{-# LANGUAGE OverloadedStrings #-}

module Data.Geometry.Types.GeographySpec where

import qualified Data.Aeson                    as Aeson
import           Test.Hspec                    (Spec, describe, it, shouldBe)

import qualified Data.Geometry.Types.Geography as Geography

spec :: Spec
spec =
  testRoundTripBoundingBox

testRoundTripBoundingBox :: Spec
testRoundTripBoundingBox =
  describe "Simple" $
    it "Round trip BoundingBox using json" $ do
      let boundingBox = Geography.BoundingBox 1 2 3 4
      Just boundingBox `shouldBe` Aeson.decode (Aeson.encode boundingBox)
