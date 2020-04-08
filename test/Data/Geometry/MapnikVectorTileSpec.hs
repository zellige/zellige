{-# LANGUAGE OverloadedStrings #-}


module Data.Geometry.MapnikVectorTileSpec where

import qualified Control.Exception                 as Exception
import qualified Control.Monad.IO.Class            as MonadIO
import qualified Data.ByteString.Lazy              as LazyByteString
import qualified Data.ByteString.Lazy              as ByteStringLazy
import           Data.Geometry.LayerSpecHelper
import qualified Data.HashMap.Lazy                 as LazyHashMap
import qualified Data.Sequence                     as Sequence
import qualified Data.Text                         as Text
import qualified Data.Text.Encoding                as TextEncoding
import           Test.Hspec                        (Expectation, Spec, describe,
                                                    expectationFailure, it,
                                                    shouldBe, shouldContain,
                                                    shouldThrow)


import qualified Data.Geometry.MapnikVectorTile    as MapnikVectorTile
import qualified Data.Geometry.VectorTile.Geometry as VectorTileGeometry
import qualified Data.Geometry.VectorTile.Types    as VectorTileTypes

spec :: Spec
spec =
  testReadFixtures

testReadFixtures :: Spec
testReadFixtures =
  describe "all tests" $ do
    it "MVT test 001: Empty tile" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/001/tile.mvt"
      let expectations layers = LazyHashMap.size layers `shouldBe` 0
      shouldBeSuccess layersOrErr expectations
    it "MVT test 002: Tile with single point feature without id" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/002/tile.mvt"
      shouldBeSuccess layersOrErr checkLayer
    -- Default of UKNOWN if missing. https://github.com/mapbox/vector-tile-spec/blob/master/2.1/vector_tile.proto#L41
    it "MVT test 003: Tile with single point with missing geometry type" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/003/tile.mvt"
      shouldBeSuccess layersOrErr checkLayer
    it "MVT test 004: Tile with single point with missing geometry" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/004/tile.mvt"
      either (`shouldBe` "No points given!") (const (expectationFailure "Should've failed")) layersOrErr
    it "MVT test 005: Tile with single point with broken tags array" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/005/tile.mvt"
      either (`shouldBe` "Uneven number of parameters given.") (const (expectationFailure "Should've failed")) layersOrErr
    it "MVT test 006: Tile with single point with invalid GeomType" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/006/tile.mvt"
      either (`shouldBe` "Failed at 19 : Bad wireGet of Enum GeomType, unrecognized Int value is 8") (const (expectationFailure "Should've failed")) layersOrErr
    it "MVT test 007: Layer version as string instead of as an int" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/007/tile.mvt"
      either (\x -> Text.unpack x `shouldContain` "Unknown field found or failure parsing field") (const (expectationFailure "Should've failed")) layersOrErr
    it "MVT test 008: Tile layer extent encoded as string" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/008/tile.mvt"
      either (\x -> Text.unpack x `shouldContain` "Unknown field found or failure parsing field") (const (expectationFailure "Should've failed")) layersOrErr
    -- Default of 4096 if missing. https://github.com/mapbox/vector-tile-spec/blob/master/2.1/vector_tile.proto#L70
    it "MVT test 009: Tile layer extent missing" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/009/tile.mvt"
      shouldBeSuccess layersOrErr checkLayer
    it "MVT test 010: Tile layer value is encoded as int, but pretends to be string" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/010/tile.mvt"
      either (\x -> Text.unpack x `shouldContain` "Unknown field found or failure parsing field") (const (expectationFailure "Should've failed")) layersOrErr
    it "MVT test 011: Tile layer value is encoded as unknown type" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/011/tile.mvt"
      either (`shouldBe` "Value decode: No legal Value type offered") (const (expectationFailure "Should've failed")) layersOrErr
    it "MVT test 012: Unknown layer version" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/012/tile.mvt"
      let expectations layers = LazyHashMap.size layers `shouldBe` 1
      shouldBeSuccess layersOrErr expectations
    it "MVT test 013: Tile with key in table encoded as int" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/013/tile.mvt"
      either (\x -> Text.unpack x `shouldContain` "Unknown field found or failure parsing field") (const (expectationFailure "Should've failed")) layersOrErr
    it "MVT test 014: Tile layer without a name" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/014/tile.mvt"
      either (\x -> Text.unpack x `shouldContain` "Required fields missing when processing ProtoName") (const (expectationFailure "Should've failed")) layersOrErr
    -- A Vector Tile MUST NOT contain two or more layers whose name values are byte-for-byte identical.
    it "MVT test 015: Two layers with the same name" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/015/tile.mvt"
      either (`shouldBe` "Duplicate layer name [hello]") (const (expectationFailure "Should've failed")) layersOrErr
    it "MVT test 016: Valid unknown geometry" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/016/tile.mvt"
      shouldBeSuccess layersOrErr checkLayer
    it "MVT test 017: Valid point geometry" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/017/tile.mvt"
      shouldBeSuccess layersOrErr checkLayer
      let expectedMetadata = LazyHashMap.fromList [("hello", VectorTileTypes.St "world")]
      shouldBeSuccess layersOrErr (checkLayerWith (checkForPoints expectedMetadata expectedPoint))
    it "MVT test 018: Valid linestring geometry" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/018/tile.mvt"
      shouldBeSuccess layersOrErr checkLayer
      let expectedMetadata = LazyHashMap.fromList [("hello", VectorTileTypes.St "world")]
          expectedLineStrings = Sequence.singleton (VectorTileGeometry.LineString (Sequence.fromList [VectorTileGeometry.Point 2 2, VectorTileGeometry.Point 2 10, VectorTileGeometry.Point 10 10]))
      shouldBeSuccess layersOrErr (checkLayerWith (checkForLineStrings expectedMetadata expectedLineStrings))
    it "MVT test 019: Valid polygon geometry" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/019/tile.mvt"
      shouldBeSuccess layersOrErr checkLayer
      let expectedMetadata = LazyHashMap.fromList [("hello", VectorTileTypes.St "world")]
          expectedPolygons = Sequence.singleton (VectorTileGeometry.Polygon (Sequence.fromList [VectorTileGeometry.Point 3 6, VectorTileGeometry.Point 8 12, VectorTileGeometry.Point 20 34, VectorTileGeometry.Point 3 6]) Sequence.empty)
      shouldBeSuccess layersOrErr (checkLayerWith (checkForPolygons expectedMetadata expectedPolygons))
    it "MVT test 020: Valid multipoint geometry" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/020/tile.mvt"
      shouldBeSuccess layersOrErr checkLayer
      let expectedMetadata = LazyHashMap.fromList [("hello", VectorTileTypes.St "world")]
          expectedPoints = Sequence.fromList [VectorTileGeometry.Point 5 7, VectorTileGeometry.Point 3 2]
      shouldBeSuccess layersOrErr (checkLayerWith (checkForPoints expectedMetadata expectedPoints))
    it "MVT test 021: Valid multilinestring geometry" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/021/tile.mvt"
      shouldBeSuccess layersOrErr checkLayer
      let expectedMetadata = LazyHashMap.fromList [("hello", VectorTileTypes.St "world")]
          expectedLineStrings = Sequence.fromList [VectorTileGeometry.LineString (Sequence.fromList [VectorTileGeometry.Point 2 2, VectorTileGeometry.Point 2 10, VectorTileGeometry.Point 10 10]), VectorTileGeometry.LineString (Sequence.fromList [VectorTileGeometry.Point 1 1, VectorTileGeometry.Point 3 5])]
      shouldBeSuccess layersOrErr (checkLayerWith (checkForLineStrings expectedMetadata expectedLineStrings))
    it "MVT test 022: Valid multipolygon geometry" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/022/tile.mvt"
      shouldBeSuccess layersOrErr checkLayer
      let expectedMetadata = LazyHashMap.fromList [("hello", VectorTileTypes.St "world")]
          expectedPolygons = Sequence.fromList [
            VectorTileGeometry.Polygon
              (Sequence.fromList [
                VectorTileGeometry.Point 0 0, VectorTileGeometry.Point 10 0, VectorTileGeometry.Point 10 10, VectorTileGeometry.Point 0 10, VectorTileGeometry.Point 0 0])
              Sequence.empty,
            VectorTileGeometry.Polygon
              (Sequence.fromList [
                VectorTileGeometry.Point 11 11, VectorTileGeometry.Point 20 11, VectorTileGeometry.Point 20 20, VectorTileGeometry.Point 11 20, VectorTileGeometry.Point 11 11])
              (Sequence.fromList [
                VectorTileGeometry.Polygon
                  (Sequence.fromList [
                    VectorTileGeometry.Point 13 13, VectorTileGeometry.Point 13 17, VectorTileGeometry.Point 17 17, VectorTileGeometry.Point 17 13, VectorTileGeometry.Point 13 13]
                  )
                Sequence.empty
              ])
            ]
      shouldBeSuccess layersOrErr (checkLayerWith (checkForPolygons expectedMetadata expectedPolygons))
    it "MVT test 023: Invalid layer: missing layer name" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/023/tile.mvt"
      either (\x -> Text.unpack x `shouldContain` "Required fields missing when processing ProtoName") (const (expectationFailure "Should've failed")) layersOrErr
    it "MVT test 024: Missing layer version" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/024/tile.mvt"
      either (\x -> Text.unpack x `shouldContain` "Required fields missing when processing ProtoName") (const (expectationFailure "Should've failed")) layersOrErr
    it "MVT test 025: Layer without features" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/025/tile.mvt"
      either (`shouldBe` "VectorTile.features: `[RawFeature]` empty") (const (expectationFailure "Should've failed")) layersOrErr
    it "MVT test 026: Extra value type" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/026/tile.mvt"
      shouldBeSuccess layersOrErr (checkNamedLayerWith "howdy" (basicLayerChecks "howdy" 2 1))
      shouldBeSuccess layersOrErr (checkNamedLayerWith "howdy" (checkForPointsNoMetadata expectedPoint))
    it "MVT test 027: Layer with unused bool property value" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/027/tile.mvt"
      shouldBeSuccess layersOrErr checkLayer
      shouldBeSuccess layersOrErr (checkLayerWith (checkForPointsNoMetadata expectedPoint))
    it "MVT test 030: Two geometry fields" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/030/tile.mvt"
      either (`shouldBe` "Invalid command found in Point feature: MoveTo (fromList [Point {x = 0, y = 0}])") (const (expectationFailure "Should've failed")) layersOrErr
    it "MVT test 032: Layer with single feature with string property value" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/032/tile.mvt"
      shouldBeSuccess layersOrErr checkLayer
      let expectedMetadata = LazyHashMap.fromList [("key1", VectorTileTypes.St "i am a string value")]
      shouldBeSuccess layersOrErr (checkLayerWith (checkForPoints expectedMetadata expectedPoint))
    it "MVT test 033: Layer with single feature with float property value" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/033/tile.mvt"
      shouldBeSuccess layersOrErr checkLayer
      let expectedMetadata = LazyHashMap.fromList [("key1", VectorTileTypes.Fl 3.1)]
      shouldBeSuccess layersOrErr (checkLayerWith (checkForPoints expectedMetadata expectedPoint))
    it "MVT test 034: Layer with single feature with double property value" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/034/tile.mvt"
      shouldBeSuccess layersOrErr checkLayer
      let expectedMetadata = LazyHashMap.fromList [("key1", VectorTileTypes.Do 1.23)]
      shouldBeSuccess layersOrErr (checkLayerWith (checkForPoints expectedMetadata expectedPoint))
    it "MVT test 035: Layer with single feature with int property value" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/035/tile.mvt"
      shouldBeSuccess layersOrErr checkLayer
      let expectedMetadata = LazyHashMap.fromList [("key1", VectorTileTypes.I64 6)]
      shouldBeSuccess layersOrErr (checkLayerWith (checkForPoints expectedMetadata expectedPoint))
    it "MVT test 036: Layer with single feature with uint property value" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/036/tile.mvt"
      shouldBeSuccess layersOrErr checkLayer
      let expectedMetadata = LazyHashMap.fromList [("key1", VectorTileTypes.W64 87948)]
      shouldBeSuccess layersOrErr (checkLayerWith (checkForPoints expectedMetadata expectedPoint))
    it "MVT test 037: Layer with single feature with sint property value" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/037/tile.mvt"
      shouldBeSuccess layersOrErr checkLayer
      let expectedMetadata = LazyHashMap.fromList [("key1", VectorTileTypes.S64 87948)]
      shouldBeSuccess layersOrErr (checkLayerWith (checkForPoints expectedMetadata expectedPoint))
    it "MVT test 038: Layer with all types of property value" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/038/tile.mvt"
      shouldBeSuccess layersOrErr checkLayer
      let expectedMetadata = LazyHashMap.fromList [
            ("float_value", VectorTileTypes.Fl 3.1),
            ("double_value", VectorTileTypes.Do 1.23),
            ("int_value", VectorTileTypes.I64 6),
            ("uint_value", VectorTileTypes.W64 87948),
            ("sint_value", VectorTileTypes.S64 (-87948)),
            ("bool_value", VectorTileTypes.B True),
            ("string_value", VectorTileTypes.St "ello")
            ]
      shouldBeSuccess layersOrErr (checkLayerWith (checkForPoints expectedMetadata expectedPoint))
    -- Default version is 1 https://github.com/mapbox/vector-tile-spec/blob/master/2.1/vector_tile.proto#L55
    it "MVT test 039: Default values are actually encoded in the tile" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/039/tile.mvt"
      shouldBeSuccess layersOrErr (checkLayerWith (basicLayerChecks "hello" 1 1))
    it "MVT test 040: Feature has tags that point to non-existent Key in the layer." $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/040/tile.mvt"
      shouldBeSuccess layersOrErr checkLayer
      let
        badMetadata :: VectorTileTypes.Layer -> IO ()
        badMetadata layer = do
            let (h Sequence.:<| _) = VectorTileTypes._points layer
            Exception.evaluate (VectorTileTypes._metadata h) `shouldThrow` errorCallContains "index out of bounds in call to: Data.Sequence.index 2"
      shouldBeSuccess layersOrErr (checkLayerWith badMetadata)
    it "MVT test 042: Feature has tags that point to non-existent Value in the layer." $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/042/tile.mvt"
      Exception.evaluate layersOrErr `shouldThrow` errorCallContains "index out of bounds in call to: Data.Sequence.index 2"
    it "MVT test 043: A layer with six points that all share the same key but each has a unique value." $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/043/tile.mvt"
      shouldBeSuccess layersOrErr (checkNamedLayerWith "park_features" (basicLayerChecks "park_features" 2 6))
      let expectedPoints = Sequence.fromList [
            Sequence.fromList [VectorTileGeometry.Point 25 17],
            Sequence.fromList [VectorTileGeometry.Point 26 19],
            Sequence.fromList [VectorTileGeometry.Point 27 15],
            Sequence.fromList [VectorTileGeometry.Point 60 10],
            Sequence.fromList [VectorTileGeometry.Point 44 20],
            Sequence.fromList [VectorTileGeometry.Point 23 49]]
          expectedMetadata = Sequence.fromList [
            LazyHashMap.fromList [("poi", VectorTileTypes.St "swing")],
            LazyHashMap.fromList [("poi", VectorTileTypes.St "water_fountain")],
            LazyHashMap.fromList [("poi", VectorTileTypes.St "slide")],
            LazyHashMap.fromList [("poi", VectorTileTypes.St "bathroom")],
            LazyHashMap.fromList [("poi", VectorTileTypes.St "tree")],
            LazyHashMap.fromList [("poi", VectorTileTypes.St "bench")]]
      shouldBeSuccess layersOrErr (checkNamedLayerWith "park_features" (checkForPointsInFeatures expectedMetadata expectedPoints))
    it "MVT test 044: Geometry field begins with a ClosePath command, which is invalid" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/044/tile.mvt"
      Exception.evaluate layersOrErr `shouldThrow` errorCallContains "LineTo Requires 2 Paramters"
    it "MVT test 045: Invalid point geometry that includes a MoveTo command and only half of the xy coordinates" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/045/tile.mvt"
      Exception.evaluate layersOrErr `shouldThrow` errorCallContains "MoveTo Requires 2 Paramters"
    it "MVT test 046: Invalid linestring geometry that includes two points in the same position, which is not OGC valid" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/046/tile.mvt"
      let expectedMetadata = LazyHashMap.fromList []
          expectedLineStrings = Sequence.singleton (VectorTileGeometry.LineString (Sequence.fromList [VectorTileGeometry.Point 2 2, VectorTileGeometry.Point 2 10]))
      shouldBeSuccess layersOrErr (checkLayerWith (checkForLineStrings expectedMetadata expectedLineStrings))
    it "MVT test 047: Invalid point geometry that includes a MoveTo command and only half of the xy coordinates" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/047/tile.mvt"
      Exception.evaluate layersOrErr `shouldThrow` errorCallContains "ClosePath was given a parameter count: 2"
    it "MVT test 048: Invalid polygon with wrong ClosePath count 0 (must be count 1)" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/048/tile.mvt"
      Exception.evaluate layersOrErr `shouldThrow` errorCallContains "ClosePath was given a parameter count: 0"
    it "MVT test 049: decoding linestring with int32 overflow in x coordinate" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/049/tile.mvt"
      let expectedMetadata = LazyHashMap.fromList []
          expectedLineStrings = Sequence.singleton (VectorTileGeometry.LineString (Sequence.fromList [VectorTileGeometry.Point 2147483647 0, VectorTileGeometry.Point 2147483648 1]))
      shouldBeSuccess layersOrErr (checkLayerWith (checkForLineStrings expectedMetadata expectedLineStrings))
    it "MVT test 050: decoding linestring with int32 overflow in y coordinate" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/050/tile.mvt"
      let expectedMetadata = LazyHashMap.fromList []
          expectedLineStrings = Sequence.singleton (VectorTileGeometry.LineString (Sequence.fromList [VectorTileGeometry.Point 0 (-2147483648), VectorTileGeometry.Point (-1) (-2147483649)]))
      shouldBeSuccess layersOrErr (checkLayerWith (checkForLineStrings expectedMetadata expectedLineStrings))
    -- This just passes - no error
    it "MVT test 051: multipoint with a huge count value, useful for ensuring no over-allocation errors." $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/051/tile.mvt"
      shouldBeSuccess layersOrErr checkLayer
    it "MVT test 052: multipoint with not enough points" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/052/tile.mvt"
      Exception.evaluate layersOrErr `shouldThrow` errorCallContains "MoveTo Requires 2 Paramters"
    it "MVT test 053: clipped square (exact extent): a polygon that covers the entire tile to the exact boundary" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/053/tile.mvt"
      let expectedMetadata = LazyHashMap.fromList [("type", VectorTileTypes.St "exact extent")]
          expectedPolygons = Sequence.fromList [
            VectorTileGeometry.Polygon
              (Sequence.fromList [
                VectorTileGeometry.Point 0 0, VectorTileGeometry.Point 4096 0, VectorTileGeometry.Point 4096 4096, VectorTileGeometry.Point 0 4096, VectorTileGeometry.Point 0 0])
              Sequence.empty
            ]
      shouldBeSuccess layersOrErr (checkNamedLayerWith "clipped-square" (checkForPolygons expectedMetadata expectedPolygons))
    it "MVT test 054: clipped square (one unit buffer): a polygon that covers the entire tile plus a one unit buffer" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/054/tile.mvt"
      let expectedMetadata = LazyHashMap.fromList [("type", VectorTileTypes.St "one unit buffer")]
          expectedPolygons = Sequence.fromList [
            VectorTileGeometry.Polygon
              (Sequence.fromList [
                VectorTileGeometry.Point (-1) (-1), VectorTileGeometry.Point 4097 (-1), VectorTileGeometry.Point 4097 4097, VectorTileGeometry.Point (-1) 4097, VectorTileGeometry.Point (-1) (-1)])
              Sequence.empty
            ]
      shouldBeSuccess layersOrErr (checkNamedLayerWith "clipped-square" (checkForPolygons expectedMetadata expectedPolygons))
    it "MVT test 055: clipped square (minus one unit buffer): a polygon that almost covers the entire tile minus one unit buffer" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/055/tile.mvt"
      let expectedMetadata = LazyHashMap.fromList [("type", VectorTileTypes.St "almost a clipped-square minus one unit")]
          expectedPolygons = Sequence.fromList [
            VectorTileGeometry.Polygon
              (Sequence.fromList [
                VectorTileGeometry.Point 1 1, VectorTileGeometry.Point 4095 1, VectorTileGeometry.Point 4095 4095, VectorTileGeometry.Point 1 4095, VectorTileGeometry.Point 1 1])
              Sequence.empty
            ]
      shouldBeSuccess layersOrErr (checkNamedLayerWith "clipped-square" (checkForPolygons expectedMetadata expectedPolygons))
    it "MVT test 056: clipped square (large buffer): a polygon that covers the entire tile plus a 200 unit buffer" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/056/tile.mvt"
      let expectedMetadata = LazyHashMap.fromList [("type", VectorTileTypes.St "large 200 unit buffer")]
          expectedPolygons = Sequence.fromList [
            VectorTileGeometry.Polygon
              (Sequence.fromList [
                VectorTileGeometry.Point (-200) (-200), VectorTileGeometry.Point 4296 (-200), VectorTileGeometry.Point 4296 4296, VectorTileGeometry.Point (-200) 4296, VectorTileGeometry.Point (-200) (-200)])
              Sequence.empty
            ]
      shouldBeSuccess layersOrErr (checkNamedLayerWith "clipped-square" (checkForPolygons expectedMetadata expectedPolygons))
    it "MVT test 057: A point fixture with a gigantic MoveTo command. Can be used to test decoders for memory overallocation situations" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/057/tile.mvt"
      shouldBeSuccess layersOrErr checkLayer
    it "MVT test 058: A linestring fixture with a gigantic LineTo command" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/058/tile.mvt"
      shouldBeSuccess layersOrErr checkLayer

