{-# LANGUAGE OverloadedStrings #-}

module Data.Geometry.MapnikVectorTile where

import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString                 as B (writeFile)
import qualified Data.ByteString.Lazy            as LBS (readFile)
import qualified Data.Geography.GeoJSON          as GJ
import           Data.Map                        as M
import           Data.Monoid                     ((<>))
import qualified Data.Vector                     as DV
import qualified Geography.VectorTile            as VT
import qualified Geography.VectorTile.Geometry   as VG
import qualified Geography.VectorTile.VectorTile as VVT
import qualified Options.Generic                 as OG

import           Data.Geometry.Clip
import           Data.Geometry.GeoJsonToMvt
import           Data.Geometry.Types

writeLayer :: LayerConfig OG.Unwrapped -> IO ()
writeLayer lc = do
    let config = configFromLayerConfig lc
    inputLayer <- readLayer (_layerInput lc) config
    let outputLayer = VT.encode . VT.untile $ VVT.VectorTile (M.fromList [(_layerName lc, inputLayer)])
    B.writeFile (_layerOutput lc) outputLayer

readLayer :: FilePath -> Config -> IO VT.Layer
readLayer filePath config = do
    geoJson <- liftIO $ readGeoJson filePath
    pure $ createMvt config geoJson

createMvt :: Config -> GJ.FeatureCollection -> VT.Layer
createMvt config geoJson = do
    let (p, l, o) = getFeatures config geoJson
        (extent, version, name) = (,,) <$> _extents <*> _version <*> _name $ config
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
