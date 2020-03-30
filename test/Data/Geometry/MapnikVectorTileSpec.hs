{-# LANGUAGE OverloadedStrings #-}

module Data.Geometry.MapnikVectorTileSpec where

import qualified Control.Error.Util                as ErrorUtil
import qualified Control.Monad                     as ControlMonad
import qualified Control.Monad.IO.Class            as MonadIO
import qualified Control.Monad.Trans.Maybe         as TransMaybe
import qualified Data.ByteString.Lazy              as LazyByteString
import qualified Data.HashMap.Lazy                 as LazyHashMap
import qualified Data.Sequence                     as Sequence
import qualified Data.Text                         as Text
import           Test.Hspec                        (Expectation, Spec, describe,
                                                    expectationFailure, it,
                                                    shouldBe, shouldContain)


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
    it "MVT test 003: Tile with single point with missing geometry type" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/003/tile.mvt"
      either (`shouldBe` "Geometry type of UNKNOWN given.") (const (expectationFailure "Should've failed")) layersOrErr
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
      either (\x -> Text.unpack x `shouldContain` "Required fields missing when processing ProtoName {protobufName = FIName") (const (expectationFailure "Should've failed")) layersOrErr
    it "MVT test 015: Two layers with the same name" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/015/tile.mvt"
      shouldBeSuccess layersOrErr checkLayer
    it "MVT test 016: Valid unknown geometry" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/016/tile.mvt"
      either (`shouldBe` "Geometry type of UNKNOWN given.") (const (expectationFailure "Should've failed")) layersOrErr
    it "MVT test 017: Valid point geometry" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/017/tile.mvt"
      shouldBeSuccess layersOrErr checkLayer
      shouldBeSuccess layersOrErr (checkLayerWith checkForPoint)

shouldBeSuccess :: Either Text.Text t -> (t -> Expectation) -> Expectation
shouldBeSuccess layersOrErr expectations =
  either (expectationFailure . Text.unpack) expectations layersOrErr

getLayers :: FilePath -> IO (Either Text.Text (LazyHashMap.HashMap LazyByteString.ByteString VectorTileTypes.Layer))
getLayers file = do
  inputTile <- MapnikVectorTile.readMvt file
  pure $ fmap VectorTileTypes._layers inputTile

checkLayer :: LazyHashMap.HashMap LazyByteString.ByteString VectorTileTypes.Layer -> Expectation
checkLayer = checkLayerWith basicLayerChecks

checkLayerWith :: (VectorTileTypes.Layer -> IO ()) -> LazyHashMap.HashMap LazyByteString.ByteString VectorTileTypes.Layer -> Expectation
checkLayerWith checks layers = ControlMonad.void $ TransMaybe.runMaybeT $ do
    MonadIO.liftIO $ LazyHashMap.size layers `shouldBe` 1
    layer <- ErrorUtil.hoistMaybe (LazyHashMap.lookup "hello" layers)
    MonadIO.liftIO $ checks layer

basicLayerChecks :: VectorTileTypes.Layer -> IO ()
basicLayerChecks layer = do
  VectorTileTypes._name layer `shouldBe` "hello"
  VectorTileTypes._version layer `shouldBe` 2
  VectorTileTypes._extent layer `shouldBe` 4096
  VectorTileTypes.numberOfFeatures layer `shouldBe` 1

checkForPoint :: VectorTileTypes.Layer -> IO ()
checkForPoint layer = do
  let expectedSeq = Sequence.singleton (VectorTileGeometry.Point 25 17)
      expectedMetadata = LazyHashMap.fromList [("hello", VectorTileTypes.St "world")]
      expectedPoints = Sequence.singleton (VectorTileTypes.Feature 1 expectedMetadata expectedSeq)
  VectorTileTypes._points layer `shouldBe` expectedPoints
  VectorTileTypes._linestrings layer `shouldBe` Sequence.empty
  VectorTileTypes._polygons layer `shouldBe` Sequence.empty


