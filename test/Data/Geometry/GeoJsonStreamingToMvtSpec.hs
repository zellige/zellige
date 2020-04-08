{-# LANGUAGE OverloadedStrings #-}


module Data.Geometry.GeoJsonStreamingToMvtSpec where

import qualified Control.Foldl                       as Foldl
import qualified Data.Aeson.Types                    as AesonTypes
import qualified Data.ByteString                     as ByteString
import qualified Data.ByteString.Lazy                as ByteStringLazy
import qualified Data.HashMap.Lazy                   as HashMapLazy
import qualified Data.HashMap.Strict                 as HashMapStrict
import qualified Data.Sequence                       as Sequence
import qualified System.IO                           as IO
import qualified System.IO.Temp                      as IOTemp
import           Test.Hspec                          (Expectation, Spec,
                                                      describe, it, shouldBe)

import qualified Data.Geospatial                     as Geospatial

import qualified Data.Geometry.GeoJsonStreamingToMvt as GeoJsonStreamingToMvt
import           Data.Geometry.LayerSpecHelper
import qualified Data.Geometry.Types.Config          as TypesConfig
import qualified Data.Geometry.Types.Geography       as TypesGeography
import qualified Data.Geometry.VectorTile.Types      as VectorTileTypes

spec :: Spec
spec =
  testWriteFixtures

config :: TypesConfig.Config
config = TypesConfig.mkConfig "hello" 1 (2,3) TypesGeography.defaultBuffer (Just 4096) 1 TypesConfig.NoAlgorithm

testWriteFixtures :: Spec
testWriteFixtures =
  describe "all tests" $ do
    it "MVT test 001: Empty tile" $ do
      let stream = Foldl.fold GeoJsonStreamingToMvt.foldStreamingLayer Sequence.empty
          tile = GeoJsonStreamingToMvt.vtToBytes config stream
          expectations layers = HashMapLazy.size layers `shouldBe` 0
      checkWriteTile tile expectations
    it "MVT test 002: Tile with single point feature without id" $ do
      let stream = Foldl.fold GeoJsonStreamingToMvt.foldStreamingLayer (Sequence.singleton (Geospatial.Point . Geospatial.GeoPoint . Geospatial.GeoPointXY $ Geospatial.PointXY 25 17,  AesonTypes.Object $ HashMapStrict.fromList [( "hello", AesonTypes.String "world")]))
          tile = GeoJsonStreamingToMvt.vtToBytes config stream
          expectedMetadata = HashMapLazy.fromList [("hello", VectorTileTypes.St "world")]
          foo = checkLayerWith (checkForPointsAt 0 expectedMetadata expectedPoint)
      checkWriteTile tile foo

checkWriteTile :: ByteString.ByteString -> (HashMapStrict.HashMap ByteStringLazy.ByteString VectorTileTypes.Layer -> Expectation) -> IO ()
checkWriteTile tile expectations = IOTemp.withSystemTempFile "tile" $ \_ h -> do
  IO.hClose h
  _ <- ByteString.writeFile "tile.mvt" tile
  layersOrErr <- getLayers "tile.mvt"
  shouldBeSuccess layersOrErr expectations
