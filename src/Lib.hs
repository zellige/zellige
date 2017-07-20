{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Lazy            as LBS (readFile)
import           Data.Geography.GeoJSON          as GJ
import           Data.Monoid                     ((<>))
import           Data.Text
import qualified Data.Vector                     as DV
import qualified Geography.VectorTile            as V
import qualified Geography.VectorTile.Geometry   as VG
import qualified Geography.VectorTile.VectorTile as VT

import           Clip
import           GeoJsonToMvt

someFunc :: IO ()
someFunc = putStrLn "someFunc"

testFile :: IO VT.Layer
testFile = readLayer "./test/integration/19781.json" 2 "foobar" 2048

readLayer :: FilePath -> Int -> Text -> Int -> IO VT.Layer
readLayer file version name extent = do
    geoJson <- liftIO $ readGeoJson file
    let (p, l, o) = getFeatures geoJson
        bb = createBoundingBoxPts extent
        cP = DV.map (\f -> f { VT._geometries = clipPoints bb (VT._geometries f) } ) p
        cL = DV.map (\f -> f { VT._geometries = clipLines bb (VT._geometries f) } ) l
        cO = DV.map (\f -> f { VT._geometries = clipPolygons bb (VT._geometries f) } ) o
    pure (VT.Layer version name p l o extent)

getFeatures :: GJ.FeatureCollection
     -> (DV.Vector (VT.Feature VG.Point), DV.Vector (VT.Feature VG.LineString), DV.Vector (VT.Feature VG.Polygon))
getFeatures = geoJsonFeaturesToMvtFeatures . features

readGeoJson :: FilePath -> IO FeatureCollection
readGeoJson geoJsonFile = do
    bs <- LBS.readFile geoJsonFile
    let ebs = eitherDecode bs :: Either String GJ.FeatureCollection
        decodeError = error . (("Unable to decode " <> geoJsonFile <> ": ") <>)
    pure (either decodeError id ebs)
