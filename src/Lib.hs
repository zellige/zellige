{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Lazy            as LBS (readFile)
import qualified Data.Geography.GeoJSON          as GJ
import           Data.Monoid                     ((<>))
import           Data.Text
import qualified Data.Vector                     as DV
import qualified Geography.VectorTile.Geometry   as VG
import qualified Geography.VectorTile.VectorTile as VT

import           Clip
import           GeoJsonToMvt
import           Types

someFunc :: IO ()
someFunc = putStrLn "someFunc"

testFile :: IO VT.Layer
testFile = readLayer "./test/integration/19781.json" 15 (28999, 19781) 2048 "foobar"

readLayer :: FilePath -> Integer -> (Integer, Integer) -> Int -> Text -> IO VT.Layer
readLayer file zoom (x, y) extents name = do
    geoJson <- liftIO $ readGeoJson file
    let newGtc = GoogleTileCoords zoom (Coords x y)
        config = Config newGtc (Pixels extents) name defaultVersion
    pure $ createMvt config geoJson

createMvt :: Config -> GJ.FeatureCollection -> VT.Layer
createMvt config geoJson = do
    let (p, l, o) = getFeatures config geoJson
        extent = _extents config
        version = _version config
        name = _name config
        clipBb = createBoundingBoxPts extent
        cG convF startGeom = startGeom { VT._geometries = convF clipBb (VT._geometries startGeom) }
        cP = DV.map (cG clipPoints) p
        cL = DV.map (cG clipLines) l
        cO = DV.map (cG clipPolygons) o
    VT.Layer version name cP cL cO (_pixels extent)

getFeatures :: Config -> GJ.FeatureCollection -> (DV.Vector (VT.Feature VG.Point), DV.Vector (VT.Feature VG.LineString), DV.Vector (VT.Feature VG.Polygon))
getFeatures config = geoJsonFeaturesToMvtFeatures config . GJ.features

readGeoJson :: FilePath -> IO GJ.FeatureCollection
readGeoJson geoJsonFile = do
    bs <- LBS.readFile geoJsonFile
    let ebs = eitherDecode bs :: Either String GJ.FeatureCollection
        decodeError = error . (("Unable to decode " <> geoJsonFile <> ": ") <>)
    pure (either decodeError id ebs)
