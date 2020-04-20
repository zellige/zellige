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
import qualified Data.Geometry.VectorTile.Types      as VectorTileTypes

spec :: Spec
spec =
  testWriteFixtures

config :: TypesConfig.Config
config = TypesConfig.mkConfig "hello" 1 (2,3) TypesGeography.defaultBuffer (Just 4096) 1 TypesConfig.NoAlgorithm

noExtentConfig :: TypesConfig.Config
noExtentConfig = TypesConfig.mkConfig "hello" 1 (2,3) TypesGeography.defaultBuffer Nothing 1 TypesConfig.NoAlgorithm

metadata :: AesonTypes.Value
metadata = AesonTypes.Object $ HashMapStrict.fromList [( "hello", AesonTypes.String "world")]

testWriteFixtures :: Spec
testWriteFixtures =
  describe "all tests" $ do
    it "MVT test 001: Empty tile" $ do
      let stream = Foldl.fold GeoJsonStreamingToMvt.foldStreamingLayer Sequence.empty
          tile = GeoJsonStreamingToMvt.vtToBytes config stream
          expectations layers = HashMapLazy.size layers `shouldBe` 0
      checkWriteTile tile expectations
    it "MVT test 009: Tile layer extent missing" $ do
      let stream = Foldl.fold GeoJsonStreamingToMvt.foldStreamingLayer (Sequence.singleton (Geospatial.Point . Geospatial.GeoPoint $ mkGeoPoint 25 17, AesonTypes.Null))
          tile = GeoJsonStreamingToMvt.vtToBytes noExtentConfig stream
          checkPoints = checkLayerWith (checkForPoints emptyMetadata expectedPoint)
      checkWriteTile tile checkPoints
      checkWriteTile tile checkLayer
    it "MVT test 017: Tile layer extent missing" $ do
      let point = Geospatial.Point . Geospatial.GeoPoint $ mkGeoPoint 25 17
          stream = Foldl.fold GeoJsonStreamingToMvt.foldStreamingLayer (Sequence.singleton (point, metadata))
          tile = GeoJsonStreamingToMvt.vtToBytes noExtentConfig stream
          checkPoints = checkLayerWith (checkForPoints expectedMetadata expectedPoint)
      checkWriteTile tile checkPoints
      checkWriteTile tile checkLayer
    it "MVT test 018: Valid linestring geometry" $ do
      let linestring = Geospatial.Line (Geospatial.GeoLine $ LineString.makeLineString (mkGeoPoint 2 2) (mkGeoPoint 2 10) (Sequence.singleton $ mkGeoPoint 10 10))
          stream = Foldl.fold GeoJsonStreamingToMvt.foldStreamingLayer (Sequence.singleton (linestring, metadata))
          tile = GeoJsonStreamingToMvt.vtToBytes noExtentConfig stream
          checkLine = checkLayerWith (checkForLineStrings expectedMetadata expectedLineStrings)
      checkWriteTile tile checkLine
      checkWriteTile tile checkLayer
    it "MVT test 019: Valid polygon geometry" $ do
      let poly = Geospatial.Polygon . Geospatial.GeoPolygon . Sequence.singleton $ LinearRing.makeLinearRing (mkGeoPoint 3 6) (mkGeoPoint 8 12) (mkGeoPoint 20 34) (Sequence.singleton (mkGeoPoint 3 6))
          stream = Foldl.fold GeoJsonStreamingToMvt.foldStreamingLayer (Sequence.fromList [(poly, metadata)])
          tile = GeoJsonStreamingToMvt.vtToBytes noExtentConfig stream
          checkPolygons = checkLayerWith (checkForPolygons expectedMetadata expectedPolygon)
      checkWriteTile tile checkPolygons
      checkWriteTile tile checkLayer
    it "MVT test 020: Valid multipoint geometry" $ do
      let points = Geospatial.MultiPoint . Geospatial.GeoMultiPoint $ Sequence.fromList [mkGeoPoint 5 7, mkGeoPoint 3 2]
          stream = Foldl.fold GeoJsonStreamingToMvt.foldStreamingLayer (Sequence.fromList [(points, metadata)])
          tile = GeoJsonStreamingToMvt.vtToBytes noExtentConfig stream
          checkPoints = checkLayerWith (checkForPoints expectedMetadata expectedMultiPoint)
      checkWriteTile tile checkPoints
      checkWriteTile tile checkLayer
    it "MVT test 021: Valid multilinestring geometry" $ do
      let line = Geospatial.MultiLine . Geospatial.GeoMultiLine $ Sequence.fromList [LineString.makeLineString (mkGeoPoint 2 2) (mkGeoPoint 2 10) (Sequence.singleton $ mkGeoPoint 10 10), LineString.makeLineString (mkGeoPoint 1 1) (mkGeoPoint 3 5) Sequence.empty]
          stream = Foldl.fold GeoJsonStreamingToMvt.foldStreamingLayer (Sequence.fromList [(line, metadata)])
          tile = GeoJsonStreamingToMvt.vtToBytes noExtentConfig stream
          checkMultiLines = checkLayerWith (checkForLineStrings expectedMetadata expectedMultiLineStrings)
      checkWriteTile tile checkMultiLines
      checkWriteTile tile checkLayer
    it "MVT test 022: Valid multipolygon geometry" $ do
      let polys = Geospatial.MultiPolygon . Geospatial.GeoMultiPolygon $ Sequence.fromList [ Sequence.fromList [LinearRing.makeLinearRing (mkGeoPoint 0 0) (mkGeoPoint 10 0) (mkGeoPoint 10 10) (Sequence.singleton (mkGeoPoint 0 10))], Sequence.fromList [ LinearRing.makeLinearRing (mkGeoPoint 11 11) (mkGeoPoint 20 11) (mkGeoPoint 20 20) (Sequence.singleton (mkGeoPoint 11 20))] , Sequence.fromList [ LinearRing.makeLinearRing (mkGeoPoint 13 13) (mkGeoPoint 13 17) (mkGeoPoint 17 17) (Sequence.singleton (mkGeoPoint 17 13))]]
          stream = Foldl.fold GeoJsonStreamingToMvt.foldStreamingLayer (Sequence.fromList [(polys, metadata)])
          tile = GeoJsonStreamingToMvt.vtToBytes noExtentConfig stream
          checkMultiPolys = checkLayerWith (checkForPolygons expectedMetadata expectedPolygons)
      checkWriteTile tile checkMultiPolys
      checkWriteTile tile checkLayer
    it "MVT test 032: Layer with single feature with string property value" $ do
      let point = Geospatial.Point . Geospatial.GeoPoint $ mkGeoPoint 25 17
          stringMetadata = AesonTypes.Object $ HashMapStrict.fromList [( "key1", AesonTypes.String "i am a string value")]
          stream = Foldl.fold GeoJsonStreamingToMvt.foldStreamingLayer (Sequence.singleton (point, stringMetadata))
          tile = GeoJsonStreamingToMvt.vtToBytes noExtentConfig stream
      checkWriteTile tile (checkLayerWith (checkForPoints expectedStringMetadata expectedPoint))

mkGeoPoint :: Double -> Double -> Geospatial.GeoPositionWithoutCRS
mkGeoPoint x y = Geospatial.GeoPointXY $ Geospatial.PointXY x y

checkWriteTile :: ByteString.ByteString -> (HashMapStrict.HashMap ByteStringLazy.ByteString VectorTileTypes.Layer -> Expectation) -> IO ()
checkWriteTile tile expectations = IOTemp.withSystemTempFile "tile" $ \_ h -> do
  IO.hClose h
  _ <- ByteString.writeFile "tile.mvt" tile
  layersOrErr <- getLayers "tile.mvt"
  shouldBeSuccess layersOrErr expectations
