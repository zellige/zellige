{-# LANGUAGE OverloadedStrings #-}

module Data.Geometry.GeoJsonStreamingToMvtSpec where

import qualified Control.Foldl                       as Foldl
import qualified Data.Aeson.Types                    as AesonTypes
import qualified Data.ByteString                     as ByteString
import qualified Data.ByteString.Lazy                as ByteStringLazy
import qualified Data.HashMap.Lazy                   as HashMapLazy
import qualified Data.HashMap.Strict                 as HashMapStrict
import qualified Data.LinearRing                     as LinearRing
import qualified Data.LineString                     as LineString
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
import qualified Data.Geometry.VectorTile.Geometry   as VectorTileGeometry
import qualified Data.Geometry.VectorTile.Types      as VectorTileTypes

spec :: Spec
spec =
  testWriteFixtures

config :: TypesConfig.Config
config = TypesConfig.mkConfig "hello" 1 (2,3) TypesGeography.defaultBuffer (Just 4096) 1 TypesConfig.NoAlgorithm

noExtentConfig :: TypesConfig.Config
noExtentConfig = TypesConfig.mkConfig "hello" 1 (2,3) TypesGeography.defaultBuffer Nothing 1 TypesConfig.NoAlgorithm


testWriteFixtures :: Spec
testWriteFixtures =
  describe "all tests" $ do
    it "MVT test 001: Empty tile" $ do
      let stream = Foldl.fold GeoJsonStreamingToMvt.foldStreamingLayer Sequence.empty
          tile = GeoJsonStreamingToMvt.vtToBytes config stream
          expectations layers = HashMapLazy.size layers `shouldBe` 0
      checkWriteTile tile expectations
    it "MVT test 009: Tile layer extent missing" $ do
      let stream = Foldl.fold GeoJsonStreamingToMvt.foldStreamingLayer (Sequence.singleton (Geospatial.Point . Geospatial.GeoPoint . Geospatial.GeoPointXY $ Geospatial.PointXY 25 17, AesonTypes.Null))
          tile = GeoJsonStreamingToMvt.vtToBytes noExtentConfig stream
          expectedMetadata = HashMapLazy.empty
          checkPoints = checkLayerWith (checkForPoints expectedMetadata expectedPoint)
      checkWriteTile tile checkPoints
      checkWriteTile tile checkLayer
    it "MVT test 017: Tile layer extent missing" $ do
      let stream = Foldl.fold GeoJsonStreamingToMvt.foldStreamingLayer (Sequence.singleton (Geospatial.Point . Geospatial.GeoPoint . Geospatial.GeoPointXY $ Geospatial.PointXY 25 17, AesonTypes.Object $ HashMapStrict.fromList [( "hello", AesonTypes.String "world")]))
          tile = GeoJsonStreamingToMvt.vtToBytes noExtentConfig stream
          expectedMetadata = HashMapLazy.fromList [("hello", VectorTileTypes.St "world")]
          checkPoints = checkLayerWith (checkForPoints expectedMetadata expectedPoint)
      checkWriteTile tile checkPoints
      checkWriteTile tile checkLayer
    it "MVT test 018: Valid linestring geometry" $ do
      let stream = Foldl.fold GeoJsonStreamingToMvt.foldStreamingLayer (Sequence.singleton (Geospatial.Line (Geospatial.GeoLine $ LineString.makeLineString (Geospatial.GeoPointXY $ Geospatial.PointXY 2 2) (Geospatial.GeoPointXY $ Geospatial.PointXY 2 10) (Sequence.singleton $ Geospatial.GeoPointXY $ Geospatial.PointXY 10 10)), AesonTypes.Object $ HashMapStrict.fromList [( "hello", AesonTypes.String "world")]))
          tile = GeoJsonStreamingToMvt.vtToBytes noExtentConfig stream
          expectedMetadata = HashMapLazy.fromList [("hello", VectorTileTypes.St "world")]
          expectedLineStrings = Sequence.singleton (VectorTileGeometry.LineString (Sequence.fromList [VectorTileGeometry.Point 2 2, VectorTileGeometry.Point 2 10, VectorTileGeometry.Point 10 10]))
          checkPoints = checkLayerWith (checkForLineStrings expectedMetadata expectedLineStrings)
      checkWriteTile tile checkPoints
      checkWriteTile tile checkLayer
    it "MVT test 019: Valid polygon geometry" $ do
      let stream = Foldl.fold GeoJsonStreamingToMvt.foldStreamingLayer (Sequence.singleton (Geospatial.Polygon (Geospatial.GeoPolygon $ Sequence.singleton $ LinearRing.makeLinearRing (Geospatial.GeoPointXY $ Geospatial.PointXY 3 6) (Geospatial.GeoPointXY $ Geospatial.PointXY 8 12) (Geospatial.GeoPointXY $ Geospatial.PointXY 20 34) (Sequence.singleton (Geospatial.GeoPointXY $ Geospatial.PointXY 3 6))), AesonTypes.Object $ HashMapStrict.fromList [( "hello", AesonTypes.String "world")]))
          tile = GeoJsonStreamingToMvt.vtToBytes noExtentConfig stream
          expectedMetadata = HashMapLazy.fromList [("hello", VectorTileTypes.St "world")]
          expectedPolygons = Sequence.singleton (VectorTileGeometry.Polygon (Sequence.fromList [VectorTileGeometry.Point 3 6, VectorTileGeometry.Point 8 12, VectorTileGeometry.Point 20 34, VectorTileGeometry.Point 3 6]) Sequence.empty)
          checkPoints = checkLayerWith (checkForPolygons expectedMetadata expectedPolygons)
      checkWriteTile tile checkPoints
      checkWriteTile tile checkLayer

checkWriteTile :: ByteString.ByteString -> (HashMapStrict.HashMap ByteStringLazy.ByteString VectorTileTypes.Layer -> Expectation) -> IO ()
checkWriteTile tile expectations = IOTemp.withSystemTempFile "tile" $ \_ h -> do
  IO.hClose h
  _ <- ByteString.writeFile "tile.mvt" tile
  layersOrErr <- getLayers "tile.mvt"
  shouldBeSuccess layersOrErr expectations
