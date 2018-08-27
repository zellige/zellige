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
import qualified Data.Text                       as DataText
import qualified Data.Vector                     as DataVector
import qualified Data.Vector.Storable            as DataVectorStorable
import qualified Geography.VectorTile            as VectorTile

import qualified Data.Geometry.Clip              as DataGeometryClip
import qualified Data.Geometry.GeoJsonToMvt      as DataGeometryGeoJsonToMvt
import qualified Data.Geometry.SphericalMercator as DGS
import qualified Data.Geometry.Types.Config      as TypesConfig
import qualified Data.Geometry.Types.LayerConfig as DGTL
import qualified Data.Geometry.Types.MvtFeatures as DataGeometryTypesMvtFeatures

-- Command line

writeLayer :: DGTL.LayerConfig -> IO ()
writeLayer lc = do
    mvt <- geoJsonFileToMvt (DGTL._layerInput lc) (configFromLayerConfig lc)
    B.writeFile (DGTL._layerOutput lc) (encodeMvt mvt)

configFromLayerConfig :: DGTL.LayerConfig -> TypesConfig.Config
configFromLayerConfig DGTL.LayerConfig{..}  = TypesConfig.mkConfig _layerName _layerZoom (_layerX, _layerY) _layerBuffer _layerExtent _layerQuantizePixels _layerSimplification

geoJsonFileToMvt :: FilePath -> TypesConfig.Config -> IO VectorTile.VectorTile
geoJsonFileToMvt filePath config = do
    geoJson <- readGeoJson filePath
    createMvt config geoJson

readGeoJson :: FilePath -> IO (GJ.GeoFeatureCollection A.Value)
readGeoJson geoJsonFile = do
    bs <- LBS.readFile geoJsonFile
    let ebs         = A.eitherDecode' bs :: Either String (GJ.GeoFeatureCollection A.Value)
        decodeError = error . (("Unable to decode " <> geoJsonFile <> ": ") <>)
    pure (either decodeError id ebs)

readMvt :: FilePath -> IO VectorTile.VectorTile
readMvt filePath = do
    b <- B.readFile filePath
    let t = VectorTile.tile b
        rawDecodeError a = error ("Unable to read " <> filePath <> ": " <> DataText.unpack a)
    pure (either rawDecodeError id t)

-- Lib

encodeMvt :: VectorTile.VectorTile -> BS.ByteString
encodeMvt = VectorTile.untile

createMvt :: TypesConfig.Config -> GJ.GeoFeatureCollection A.Value -> IO VectorTile.VectorTile
createMvt TypesConfig.Config{..} geoJson = do
    let zConfig         = TypesConfig.ZoomConfig _extents _quantizePixels (DGS.boundingBox _gtc) _simplify
        clipBb          = DataGeometryClip.createBoundingBoxPts _buffer _extents
        DataGeometryTypesMvtFeatures.MvtFeatures{..} = ST.runST $ getFeatures zConfig geoJson
        cP = DF.foldl' (accNewGeom' (DataGeometryClip.clipPoints clipBb)) mempty mvtPoints
        cL = DF.foldl' (accNewGeom'' (DataGeometryClip.clipLinesCs clipBb)) mempty mvtLines
        cO = DF.foldl' (accNewGeom'' (DataGeometryClip.clipPolygons clipBb )) mempty mvtPolygons
        layer = VectorTile.Layer (fromIntegral _version) _name cP cL cO (fromIntegral _extents)
    pure . VectorTile.VectorTile $ HM.fromList [(_name, layer)]


accNewGeom' :: (DataVectorStorable.Vector VectorTile.Point -> DataVectorStorable.Vector VectorTile.Point) -> DataVector.Vector (VectorTile.Feature (DataVectorStorable.Vector VectorTile.Point)) -> VectorTile.Feature (DataVectorStorable.Vector VectorTile.Point) -> DataVector.Vector (VectorTile.Feature (DataVectorStorable.Vector VectorTile.Point))
accNewGeom' conversionFunction acc startGeom = if DataVectorStorable.null clippedGeoms then acc else DataVector.cons newGeom acc
    where
        clippedGeoms = conversionFunction $ VectorTile._geometries startGeom
        newGeom = startGeom { VectorTile._geometries = clippedGeoms }

accNewGeom'' :: (DataVector.Vector a -> DataVector.Vector a) -> DataVector.Vector (VectorTile.Feature (DataVector.Vector a)) -> VectorTile.Feature (DataVector.Vector a) -> DataVector.Vector (VectorTile.Feature (DataVector.Vector a))
accNewGeom'' conversionFunction acc startGeom = if DataVector.null clippedGeoms then acc else DataVector.cons newGeom acc
    where
        clippedGeoms = conversionFunction $ VectorTile._geometries startGeom
        newGeom = startGeom { VectorTile._geometries = clippedGeoms }

getFeatures :: TypesConfig.ZoomConfig -> GJ.GeoFeatureCollection A.Value -> ST.ST s DataGeometryTypesMvtFeatures.MvtFeatures
getFeatures extentsQBb = DataGeometryGeoJsonToMvt.geoJsonFeaturesToMvtFeatures extentsQBb . GJ._geofeatures
