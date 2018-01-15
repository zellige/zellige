{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.Geometry.MapnikVectorTile where

import qualified Control.Monad.ST                as ST
import qualified Data.Aeson                      as A
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Char8           as BS
import qualified Data.ByteString.Lazy            as LBS
import qualified Data.Foldable                   as DF
import qualified Data.Geospatial                 as GJ
import           Data.Map                        as M
import           Data.Monoid                     ((<>))
import qualified Data.Text                       as T
import qualified Data.Vector                     as DV
import qualified Geography.VectorTile            as VT
import qualified Geography.VectorTile.VectorTile as VVT

import           Data.Geometry.Clip              as DGC
import           Data.Geometry.GeoJsonToMvt      as DGG
import           Data.Geometry.SphericalMercator as DGS
import           Data.Geometry.Types.LayerConfig as DGTL
import           Data.Geometry.Types.MvtFeatures as DGMF
import           Data.Geometry.Types.Types       as DGTT

-- Command line

writeLayer :: LayerConfig -> IO ()
writeLayer lc = do
    mvt <- geoJsonFileToMvt (_layerInput lc) (configFromLayerConfig lc)
    B.writeFile (_layerOutput lc) (encodeMvt mvt)

configFromLayerConfig :: LayerConfig -> Config
configFromLayerConfig lc = mkConfig (_layerName lc) (_layerZoom lc) (_layerX lc, _layerY lc) (Pixels $ _layerBuffer lc) (Pixels $ _layerExtent lc) (Pixels $ _layerQuantizePixels lc)

geoJsonFileToMvt :: FilePath -> Config -> IO VT.VectorTile
geoJsonFileToMvt filePath config = do
    geoJson <- readGeoJson filePath
    createMvt config geoJson

readGeoJson :: FilePath -> IO (GJ.GeoFeatureCollection A.Value)
readGeoJson geoJsonFile = do
    bs <- LBS.readFile geoJsonFile
    let ebs         = A.eitherDecode' bs :: Either String (GJ.GeoFeatureCollection A.Value)
        decodeError = error . (("Unable to decode " <> geoJsonFile <> ": ") <>)
    pure (either decodeError id ebs)

readMvt :: FilePath -> IO VVT.VectorTile
readMvt filePath = do
    b <- B.readFile filePath
    let db               = VT.decode b
        rawDecodeError a = error (("Unable to read " <> filePath <> ": ") <> T.unpack a)
        x                = either rawDecodeError id db
        t                = VT.tile x
    pure (either rawDecodeError id t)

-- Lib

encodeMvt :: VT.VectorTile -> BS.ByteString
encodeMvt = VT.encode . VT.untile

createMvt :: Config -> GJ.GeoFeatureCollection A.Value -> IO VT.VectorTile
createMvt Config{..} geoJson = do
    let zconfig         = ZoomConfig _extents _quantizePixels (boundingBox _gtc)
        clipBb          = createBoundingBoxPts _buffer _extents
        MvtFeatures{..} = ST.runST $ getFeatures zconfig geoJson
        cP = DF.foldl' (accNewGeom (clipPoints clipBb)) DV.empty mvtPoints
        cL = DF.foldl' (accNewGeom (clipLines clipBb)) DV.empty mvtLines
        cO = DF.foldl' (accNewGeom (clipPolygons clipBb )) DV.empty mvtPolygons
        iExtents = (fromIntegral . toInteger . _pixels) _extents
        layer = VT.Layer _version _name cP cL cO iExtents
    pure . VT.VectorTile $ M.fromList [(_name, layer)]

accNewGeom :: (DV.Vector a -> DV.Vector a) -> DV.Vector (VVT.Feature a) -> VVT.Feature a -> DV.Vector (VVT.Feature a)
accNewGeom convF acc startGeom = if DV.null genClip then acc else DV.cons newGeom acc
    where
        genClip = convF (VT._geometries startGeom)
        newGeom = startGeom { VT._geometries = genClip }

getFeatures :: ZoomConfig -> GJ.GeoFeatureCollection A.Value -> ST.ST s MvtFeatures
getFeatures extentsQBb = geoJsonFeaturesToMvtFeatures extentsQBb . GJ._geofeatures
