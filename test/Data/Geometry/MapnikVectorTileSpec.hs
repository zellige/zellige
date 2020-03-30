{-# LANGUAGE OverloadedStrings #-}

module Data.Geometry.MapnikVectorTileSpec where

import qualified Control.Error.Util             as ErrorUtil
import qualified Control.Monad                  as ControlMonad
import qualified Control.Monad.IO.Class         as MonadIO
import qualified Control.Monad.Trans.Maybe      as TransMaybe
import qualified Data.ByteString.Lazy           as LazyByteString
import qualified Data.HashMap.Lazy              as LazyHashMap
import qualified Data.Text                      as Text
import           Test.Hspec                     (Expectation, Spec, describe,
                                                 expectationFailure, it,
                                                 shouldBe)


import qualified Data.Geometry.MapnikVectorTile as MapnikVectorTile
import qualified Data.Geometry.VectorTile.Types as VectorTileTypes

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
      let expectations layers = do
            LazyHashMap.size layers `shouldBe` 1
            checkLayer layers
      shouldBeSuccess layersOrErr expectations
    it "MVT test 003: Tile with single point with missing geometry type" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/003/tile.mvt"
      either (`shouldBe` "Geometry type of UNKNOWN given.") (const (expectationFailure "Should've failed")) layersOrErr
    it "MVT test 004: Tile with single point with missing geometry" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/004/tile.mvt"
      either (`shouldBe` "No points given!") (const (expectationFailure "Should've failed")) layersOrErr
    it "MVT test 005: Tile with single point with broken tags array" $ do
      layersOrErr <- getLayers "./test/mvt-fixtures/fixtures/005/tile.mvt"
      either (`shouldBe` "Uneven number of parameters given.") (const (expectationFailure "Should've failed")) layersOrErr

shouldBeSuccess :: Either Text.Text t -> (t -> Expectation) -> Expectation
shouldBeSuccess layersOrErr expectations =
  either (expectationFailure . Text.unpack) expectations layersOrErr

getLayers :: FilePath -> IO (Either Text.Text (LazyHashMap.HashMap LazyByteString.ByteString VectorTileTypes.Layer))
getLayers file = do
  inputTile <- MapnikVectorTile.readMvt file
  pure $ fmap VectorTileTypes._layers inputTile

checkLayer :: LazyHashMap.HashMap LazyByteString.ByteString VectorTileTypes.Layer -> Expectation
checkLayer layers = ControlMonad.void $ TransMaybe.runMaybeT $ do
    layer <- ErrorUtil.hoistMaybe (LazyHashMap.lookup "hello" layers)
    MonadIO.liftIO $ do
      VectorTileTypes._name layer `shouldBe` "hello"
      VectorTileTypes._version layer `shouldBe` 2
      VectorTileTypes._extent layer `shouldBe` 4096
      VectorTileTypes.numberOfFeatures layer `shouldBe` 1



