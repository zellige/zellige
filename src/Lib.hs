{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Lazy            as LBS (readFile)
import           Data.Geography.GeoJSON          as GJ
import           Data.Monoid                     ((<>))
import           Data.Text
import           Data.Vector                     as DV
import           Geography.VectorTile            as V
import           Geography.VectorTile.VectorTile as VT

import           GeoJsonToMvt

someFunc :: IO ()
someFunc = putStrLn "someFunc"

testFile :: IO VT.Layer
testFile = readLayer "./test/integration/19781.json" 2 "foobar" 2048

readLayer :: FilePath -> Int -> Text -> Int -> IO Layer
readLayer file version name extent = do
    geoJson <- liftIO $ readGeoJson file
    let (p, l, o) = (geoJsonFeaturesToMvtFeatures . features) geoJson
    pure (Layer version name (DV.fromList p) (DV.fromList l) (DV.fromList o) extent)

readGeoJson :: FilePath -> IO FeatureCollection
readGeoJson geoJsonFile = do
    bs <- LBS.readFile geoJsonFile
    let ebs = eitherDecode bs :: Either String GJ.FeatureCollection
        decodeError = error . (("Unable to decode " <> geoJsonFile <> ": ") <>)
    pure (either decodeError id ebs)
