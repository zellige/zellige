{-# LANGUAGE OverloadedStrings #-}


module Data.Geometry.GeoJsonStreamingToMvtSpec where

import qualified Control.Exception                   as Exception
import qualified Control.Foldl                       as Foldl
import qualified Control.Monad.IO.Class              as MonadIO
import qualified Data.ByteString                     as ByteString
import qualified Data.ByteString.Lazy                as LazyByteString
import           Data.Geometry.LayerSpecHelper
import qualified Data.HashMap.Lazy                   as LazyHashMap
import qualified Data.Sequence                       as Sequence
import qualified Data.Text                           as Text
import qualified Data.Text.Encoding                  as TextEncoding
import qualified System.IO                           as IO
import qualified System.IO.Temp                      as IOTemp
import           Test.Hspec                          (Expectation, Spec,
                                                      describe,
                                                      expectationFailure, it,
                                                      shouldBe, shouldContain,
                                                      shouldThrow)


import qualified Data.Geometry.GeoJsonStreamingToMvt as GeoJsonStreamingToMvt
import qualified Data.Geometry.LayerSpecHelper       as LayerSpecHelper
import qualified Data.Geometry.MapnikVectorTile      as MapnikVectorTile
import qualified Data.Geometry.Types.Config          as TypesConfig
import qualified Data.Geometry.Types.Geography       as TypesGeography
import qualified Data.Geometry.VectorTile.Geometry   as VectorTileGeometry
import qualified Data.Geometry.VectorTile.Types      as VectorTileTypes

spec :: Spec
spec =
  testWriteFixtures

testWriteFixtures :: Spec
testWriteFixtures =
  describe "all tests" $
    it "MVT test 001: Empty tile" $ do
      let config = TypesConfig.mkConfig "hello" 1 (2,3) TypesGeography.defaultBuffer 4096 1 TypesConfig.NoAlgorithm
          x = Foldl.fold GeoJsonStreamingToMvt.foldStreamingLayer Sequence.empty
          y = GeoJsonStreamingToMvt.vtToBytes config x
      IOTemp.withSystemTempFile "tile" $ \f h -> do
        IO.hClose h
        _ <- ByteString.writeFile "tile.mvt" y
        layersOrErr <- getLayers "tile.mvt"
        let expectations layers = LazyHashMap.size layers `shouldBe` 0
        shouldBeSuccess layersOrErr expectations
