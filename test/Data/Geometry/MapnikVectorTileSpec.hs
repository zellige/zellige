{-# LANGUAGE OverloadedStrings #-}

module Data.Geometry.MapnikVectorTileSpec where

import qualified Control.Error.Util             as ErrorUtil
import qualified Control.Monad                  as ControlMonad
import qualified Control.Monad.IO.Class         as MonadIO
import           Control.Monad.Trans.Maybe      as TransMaybe
import qualified Data.ByteString.Lazy           as LazyByteString
import qualified Data.HashMap.Lazy              as LazyHashMap
import           Test.Hspec                     (Expectation, Spec, describe,
                                                 it, shouldBe)


import qualified Data.Geometry.MapnikVectorTile as MapnikVectorTile
import qualified Data.Geometry.VectorTile.Types as VectorTileTypes

spec :: Spec
spec =
  testReadFixtures

testReadFixtures :: Spec
testReadFixtures =
  describe "all tests" $ do
    it "MVT test 001: Empty tile" $ do
      inputTile <- MapnikVectorTile.readMvt "./test/mvt-fixtures/fixtures/001/tile.mvt"
      LazyHashMap.size (VectorTileTypes._layers inputTile) `shouldBe` 0
    it "MVT test 002: Tile with single point feature without id" $ do
      inputTile <- MapnikVectorTile.readMvt "./test/mvt-fixtures/fixtures/002/tile.mvt"
      let layers = VectorTileTypes._layers inputTile
      LazyHashMap.size layers `shouldBe` 1
      checkLayer layers
      pure ()

checkLayer :: LazyHashMap.HashMap LazyByteString.ByteString VectorTileTypes.Layer -> Expectation
checkLayer layers = ControlMonad.void $ TransMaybe.runMaybeT $ do
    layer <- ErrorUtil.hoistMaybe (LazyHashMap.lookup "hello" layers)
    MonadIO.liftIO $ VectorTileTypes._name layer `shouldBe` "hello"
    MonadIO.liftIO $ VectorTileTypes._version layer `shouldBe` 2
    MonadIO.liftIO $ VectorTileTypes._extent layer `shouldBe` 4096
    MonadIO.liftIO $ VectorTileTypes.numberOfFeatures layer `shouldBe` 1



