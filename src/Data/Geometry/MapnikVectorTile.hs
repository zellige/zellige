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
import qualified Data.HashMap.Lazy               as HM
import           Data.Monoid                     ((<>))
import qualified Data.Sequence                   as DS
import qualified Data.Text                       as T
import qualified Geography.VectorTile            as VT

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

configFromLayerConfig :: DGTL.LayerConfig -> DGTT.Config
configFromLayerConfig DGTL.LayerConfig{..}  = DGTT.mkConfig _layerName _layerZoom (_layerX, _layerY) _layerBuffer _layerExtent _layerQuantizePixels _layerSimplification

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

readMvt :: FilePath -> IO VT.VectorTile
readMvt filePath = do
    b <- B.readFile filePath
    let t = VT.tile b
        rawDecodeError a = error ("Unable to read " <> filePath <> ": " <> T.unpack a)
    pure (either rawDecodeError id t)

-- Lib

encodeMvt :: VT.VectorTile -> BS.ByteString
encodeMvt = VT.untile

createMvt :: DGTT.Config -> GJ.GeoFeatureCollection A.Value -> IO VT.VectorTile
createMvt DGTT.Config{..} geoJson = do
    let zConfig         = DGTT.ZoomConfig _extents _quantizePixels (DGS.boundingBox _gtc) _simplify
        clipBb          = DGC.createBoundingBoxPts _buffer _extents
        DGMF.MvtFeatures{..} = ST.runST $ getFeatures zConfig geoJson
        cP = DF.foldl' (accNewGeom (DGC.clipPoints clipBb)) mempty mvtPoints
        cL = DF.foldl' (accNewGeom (DGC.clipLines clipBb)) mempty mvtLines
        cO = DF.foldl' (accNewGeom (DGC.clipPolygons clipBb )) mempty mvtPolygons
        layer = VT.Layer (fromIntegral _version) _name cP cL cO (fromIntegral _extents)
    pure . VT.VectorTile $ HM.fromList [(_name, layer)]

accNewGeom :: (DS.Seq a -> DS.Seq a) -> DS.Seq (VT.Feature a) -> VT.Feature a -> DS.Seq (VT.Feature a)
accNewGeom convF acc startGeom = if DS.null genClip then acc else newGeom DS.<| acc
    where
        genClip = convF (VT._geometries startGeom)
        newGeom = startGeom { VT._geometries = genClip }

getFeatures :: DGTT.ZoomConfig -> GJ.GeoFeatureCollection A.Value -> ST.ST s DGMF.MvtFeatures
getFeatures extentsQBb = DGG.geoJsonFeaturesToMvtFeatures extentsQBb . GJ._geofeatures
