{-# LANGUAGE OverloadedStrings #-}

module Data.Geometry.LayerSpecHelper where

import qualified Control.Exception                 as Exception
import qualified Control.Monad.IO.Class            as MonadIO
import qualified Data.ByteString.Lazy              as ByteStringLazy
import qualified Data.HashMap.Lazy                 as LazyHashMap
import qualified Data.Sequence                     as Sequence
import qualified Data.Text                         as Text
import qualified Data.Text.Encoding                as TextEncoding
import           Test.Hspec                        (Expectation,
                                                    expectationFailure,
                                                    shouldBe)

import qualified Data.Geometry.MapnikVectorTile    as MapnikVectorTile
import qualified Data.Geometry.VectorTile.Geometry as VectorTileGeometry
import qualified Data.Geometry.VectorTile.Types    as VectorTileTypes


errorCallContains :: Text.Text -> Exception.ErrorCall -> Bool
errorCallContains s (Exception.ErrorCallWithLocation msg _) = s `Text.isInfixOf` Text.pack msg

shouldBeSuccess :: Either Text.Text t -> (t -> Expectation) -> Expectation
shouldBeSuccess layersOrErr expectations =
  either (expectationFailure . Text.unpack) expectations layersOrErr

getLayers :: FilePath -> IO (Either Text.Text (LazyHashMap.HashMap ByteStringLazy.ByteString VectorTileTypes.Layer))
getLayers file = do
  inputTile <- MapnikVectorTile.readMvt file
  pure $ fmap VectorTileTypes._layers inputTile

checkLayer :: LazyHashMap.HashMap ByteStringLazy.ByteString VectorTileTypes.Layer -> Expectation
checkLayer = checkLayerWith (basicLayerChecks "hello" 2 1)

checkLayerWith :: (VectorTileTypes.Layer -> IO ()) -> LazyHashMap.HashMap ByteStringLazy.ByteString VectorTileTypes.Layer -> Expectation
checkLayerWith = checkNamedLayerWith "hello"

checkNamedLayerWith :: ByteStringLazy.ByteString -> (VectorTileTypes.Layer -> IO ()) -> LazyHashMap.HashMap ByteStringLazy.ByteString VectorTileTypes.Layer ->  Expectation
checkNamedLayerWith layerName checks layers = do
    MonadIO.liftIO $ LazyHashMap.size layers `shouldBe` 1
    let layer = LazyHashMap.lookup layerName layers
    maybe (expectationFailure (Text.unpack $ "[" <> TextEncoding.decodeUtf8 (ByteStringLazy.toStrict layerName) <> "] layer not found")) (MonadIO.liftIO . checks) layer

basicLayerChecks :: ByteStringLazy.ByteString -> Word -> Int -> VectorTileTypes.Layer -> IO ()
basicLayerChecks layerName version numberOfFeatures layer = do
  VectorTileTypes._name layer `shouldBe` layerName
  VectorTileTypes._version layer `shouldBe` version
  VectorTileTypes._extent layer `shouldBe` 4096
  VectorTileTypes.numberOfFeatures layer `shouldBe` numberOfFeatures

expectedPoint :: Sequence.Seq VectorTileGeometry.Point
expectedPoint = Sequence.singleton (VectorTileGeometry.Point 25 17)

checkForPointsNoMetadata :: Sequence.Seq VectorTileGeometry.Point -> VectorTileTypes.Layer -> IO ()
checkForPointsNoMetadata = checkForPoints LazyHashMap.empty

checkForPointsAt :: Word -> LazyHashMap.HashMap ByteStringLazy.ByteString VectorTileTypes.Val -> Sequence.Seq VectorTileGeometry.Point -> VectorTileTypes.Layer -> IO ()
checkForPointsAt startId expectedMetadata expectedSeq = checkForPointsInFeatures startId (Sequence.singleton expectedMetadata) (Sequence.singleton expectedSeq)

checkForPoints :: LazyHashMap.HashMap ByteStringLazy.ByteString VectorTileTypes.Val -> Sequence.Seq VectorTileGeometry.Point -> VectorTileTypes.Layer -> IO ()
checkForPoints expectedMetadata expectedSeq = checkForPointsInFeatures 1 (Sequence.singleton expectedMetadata) (Sequence.singleton expectedSeq)

checkForPointsInFeatures :: Word -> Sequence.Seq (LazyHashMap.HashMap ByteStringLazy.ByteString VectorTileTypes.Val) -> Sequence.Seq (Sequence.Seq VectorTileGeometry.Point) -> VectorTileTypes.Layer -> IO ()
checkForPointsInFeatures startId expectedMetadatas expectedSeqs layer = do
  let ids = Sequence.fromList $ take (Sequence.length expectedSeqs) [startId..]
      expectedPoints = Sequence.zipWith3 VectorTileTypes.Feature ids expectedMetadatas expectedSeqs
  VectorTileTypes._points layer `shouldBe` expectedPoints
  VectorTileTypes._linestrings layer `shouldBe` Sequence.empty
  VectorTileTypes._polygons layer `shouldBe` Sequence.empty

checkForLineStrings :: LazyHashMap.HashMap ByteStringLazy.ByteString VectorTileTypes.Val -> Sequence.Seq VectorTileGeometry.LineString -> VectorTileTypes.Layer -> IO ()
checkForLineStrings expectedMetadata expectedSeq = checkForLineStringsInFeatures (Sequence.singleton expectedMetadata) (Sequence.singleton expectedSeq)

checkForLineStringsInFeatures :: Sequence.Seq (LazyHashMap.HashMap ByteStringLazy.ByteString VectorTileTypes.Val) -> Sequence.Seq (Sequence.Seq VectorTileGeometry.LineString) -> VectorTileTypes.Layer -> IO ()
checkForLineStringsInFeatures expectedMetadatas expectedSeqs layer = do
  let ids = Sequence.fromList $ take (Sequence.length expectedSeqs) [1..]
      expectedLineStrings = Sequence.zipWith3 VectorTileTypes.Feature ids expectedMetadatas expectedSeqs
  VectorTileTypes._points layer `shouldBe` Sequence.empty
  VectorTileTypes._linestrings layer `shouldBe` expectedLineStrings
  VectorTileTypes._polygons layer `shouldBe` Sequence.empty

checkForPolygons :: LazyHashMap.HashMap ByteStringLazy.ByteString VectorTileTypes.Val -> Sequence.Seq VectorTileGeometry.Polygon -> VectorTileTypes.Layer -> IO ()
checkForPolygons expectedMetadata expectedSeq layer = do
  let expectedPolygon = Sequence.singleton (VectorTileTypes.Feature 1 expectedMetadata expectedSeq)
  VectorTileTypes._points layer `shouldBe` Sequence.empty
  VectorTileTypes._linestrings layer `shouldBe` Sequence.empty
  VectorTileTypes._polygons layer `shouldBe` expectedPolygon

checkForPolygonsInFeatures :: Sequence.Seq (LazyHashMap.HashMap ByteStringLazy.ByteString VectorTileTypes.Val) -> Sequence.Seq (Sequence.Seq VectorTileGeometry.Polygon) -> VectorTileTypes.Layer -> IO ()
checkForPolygonsInFeatures expectedMetadatas expectedSeqs layer = do
  let ids = Sequence.fromList $ take (Sequence.length expectedSeqs) [1..]
      expectedPolygons = Sequence.zipWith3 VectorTileTypes.Feature ids expectedMetadatas expectedSeqs
  VectorTileTypes._points layer `shouldBe` Sequence.empty
  VectorTileTypes._linestrings layer `shouldBe` Sequence.empty
  VectorTileTypes._polygons layer `shouldBe` expectedPolygons

