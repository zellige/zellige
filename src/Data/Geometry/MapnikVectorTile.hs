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

import qualified Data.Geometry.Clip              as DGC
import qualified Data.Geometry.GeoJsonToMvt      as DGG
import qualified Data.Geometry.SphericalMercator as DGS
import qualified Data.Geometry.Types.LayerConfig as DGTL
import qualified Data.Geometry.Types.MvtFeatures as DGMF
import qualified Data.Geometry.Types.Types       as DGTT

-- Command line

writeLayer :: DGTL.LayerConfig -> IO ()
writeLayer lc = do
    mvt <- geoJsonFileToMvt (DGTL._layerInput lc) (configFromLayerConfig lc)
    B.writeFile (DGTL._layerOutput lc) (encodeMvt mvt)

--    mkConfig :: Text -> ZoomLevel -> (Integer, Integer) -> Pixels -> Pixels -> Pixels -> Config

configFromLayerConfig :: DGTL.LayerConfig -> DGTT.Config
configFromLayerConfig DGTL.LayerConfig{..}  = DGTT.mkConfig _layerName _layerZoom (_layerX, _layerY) (DGTT.Pixels $ _layerBuffer) (DGTT.Pixels $ _layerExtent) (DGTT.Pixels $ _layerQuantizePixels)

geoJsonFileToMvt :: FilePath -> DGTT.Config -> IO VT.VectorTile
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

createMvt :: DGTT.Config -> GJ.GeoFeatureCollection A.Value -> IO VT.VectorTile
createMvt DGTT.Config{..} geoJson = do
    let zconfig         = DGTT.ZoomConfig _extents _quantizePixels (DGS.boundingBox _gtc)
        clipBb          = DGC.createBoundingBoxPts _buffer _extents
        DGMF.MvtFeatures{..} = ST.runST $ getFeatures zconfig geoJson
        cP = DF.foldl' (accNewGeom (DGC.clipPoints clipBb)) DV.empty mvtPoints
        cL = DF.foldl' (accNewGeom (DGC.clipLines clipBb)) DV.empty mvtLines
        cO = DF.foldl' (accNewGeom (DGC.clipPolygons clipBb )) DV.empty mvtPolygons
        iExtents = (fromIntegral . toInteger . DGTT._pixels) _extents
        layer = VT.Layer _version _name cP cL cO iExtents
    pure . VT.VectorTile $ M.fromList [(_name, layer)]

accNewGeom :: (DV.Vector a -> DV.Vector a) -> DV.Vector (VVT.Feature a) -> VVT.Feature a -> DV.Vector (VVT.Feature a)
accNewGeom convF acc startGeom = if DV.null genClip then acc else DV.cons newGeom acc
    where
        genClip = convF (VT._geometries startGeom)
        newGeom = startGeom { VT._geometries = genClip }

getFeatures :: DGTT.ZoomConfig -> GJ.GeoFeatureCollection A.Value -> ST.ST s DGMF.MvtFeatures
getFeatures extentsQBb = DGG.geoJsonFeaturesToMvtFeatures extentsQBb . GJ._geofeatures
