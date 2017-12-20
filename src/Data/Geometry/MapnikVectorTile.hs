{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.Geometry.MapnikVectorTile where

import           Control.Monad.IO.Class
import qualified Control.Monad.ST                as ST
import           Data.Aeson
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Lazy            as LBS (readFile)
import qualified Data.Geography.GeoJSON          as GJ
import           Data.Map                        as M
import           Data.Monoid                     ((<>))
import qualified Data.Text                       as T
import qualified Data.Vector                     as DV
import qualified Geography.VectorTile            as VT
import qualified Geography.VectorTile.VectorTile as VVT
import qualified Options.Generic                 as OG

import           Data.Geometry.Clip
import           Data.Geometry.GeoJsonToMvt
import           Data.Geometry.SphericalMercator
import           Data.Geometry.Types

writeLayer :: LayerConfig OG.Unwrapped -> IO ()
writeLayer lc = do
    let config = configFromLayerConfig lc
    inputLayer <- readLayer (_layerInput lc) config
    let outputLayer = VT.encode . VT.untile $ VVT.VectorTile (M.fromList [(_layerName lc, inputLayer)])
    liftIO (B.writeFile (_layerOutput lc) outputLayer)

readLayer :: FilePath -> Config -> IO VT.Layer
readLayer filePath config = do
    geoJson <- liftIO $ readGeoJson filePath
    createMvt config geoJson

createMvt :: Config -> GJ.FeatureCollection -> IO VT.Layer
createMvt Config{..} geoJson = do
    let
        MvtFeatures{..} = ST.runST $ getFeatures extentsBb geoJson
        cP = DV.foldl' (accNewGeom (clipPoints clipBb)) DV.empty mvtPoints
        cL = DV.foldl' (accNewGeom (clipLines clipBb)) DV.empty mvtLines
        cO = DV.foldl' (accNewGeom (clipPolygons clipBb)) DV.empty mvtPolygons
    pure $ VT.Layer _version _name cP cL cO (_pixels _extents)
    where
        extentsBb = (_extents, boundingBox _gtc)
        clipBb = createBoundingBoxPts _buffer _extents

accNewGeom :: (DV.Vector a -> DV.Vector a) -> DV.Vector (VVT.Feature a) -> VVT.Feature a -> DV.Vector (VVT.Feature a)
accNewGeom convF acc startGeom = if DV.null genClip then acc else DV.cons newGeom acc
    where
        genClip = convF (VT._geometries startGeom)
        newGeom = startGeom { VT._geometries = genClip }

getFeatures :: (Pixels, Data.Geometry.Types.BoundingBox) -> GJ.FeatureCollection -> ST.ST s MvtFeatures
getFeatures extentsBb = geoJsonFeaturesToMvtFeatures extentsBb . GJ.features

readGeoJson :: FilePath -> IO GJ.FeatureCollection
readGeoJson geoJsonFile = do
    bs <- LBS.readFile geoJsonFile
    let ebs = eitherDecode' bs :: Either String GJ.FeatureCollection
        decodeError = error . (("Unable to decode " <> geoJsonFile <> ": ") <>)
    pure (either decodeError id ebs)

readMvt :: FilePath -> IO VVT.VectorTile
readMvt filePath = do
    b <- B.readFile filePath
    let db = VT.decode b
        rawDecodeError a = error (("Unable to read " <> filePath <> ": ") <> T.unpack a)
        x = either rawDecodeError id db
        t = VT.tile x
    pure (either rawDecodeError id t)
