{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.Geometry.MapnikVectorTile where

import qualified Control.Monad.ST                    as ST
import qualified Data.Aeson                          as Aeson
import qualified Data.ByteString                     as ByteString
import qualified Data.ByteString.Char8               as ByteStringChar8
import qualified Data.ByteString.Lazy                as ByteStringLazy
import qualified Data.Geospatial                     as Geospatial
import qualified Data.HashMap.Lazy                   as HashMapLazy
import           Data.Monoid                         ((<>))
import qualified Data.Text                           as Text
import qualified Geography.VectorTile                as VectorTile

import qualified Data.Geometry.Clip                  as Clip
import qualified Data.Geometry.GeoJsonToMvt          as GeoJsonToMvt
import qualified Data.Geometry.Simplify              as Simplify
import qualified Data.Geometry.SphericalMercator     as SphericalMercator
import qualified Data.Geometry.Types.Config          as TypesConfig
import qualified Data.Geometry.Types.GeoJsonFeatures as TypesGeoJsonFeatures
import qualified Data.Geometry.Types.LayerConfig     as TypesLayerConfig

-- Command line

writeLayer :: TypesLayerConfig.LayerConfig -> IO ()
writeLayer lc = do
    mvt <- geoJsonFileToMvt (TypesLayerConfig._layerInput lc) (configFromLayerConfig lc)
    ByteString.writeFile (TypesLayerConfig._layerOutput lc) (encodeMvt mvt)

configFromLayerConfig :: TypesLayerConfig.LayerConfig -> TypesConfig.Config
configFromLayerConfig TypesLayerConfig.LayerConfig{..}  = TypesConfig.mkConfig _layerName _layerZoom (_layerX, _layerY) _layerBuffer _layerExtent _layerQuantizePixels _layerSimplification

geoJsonFileToMvt :: FilePath -> TypesConfig.Config -> IO VectorTile.VectorTile
geoJsonFileToMvt filePath config = do
    geoJson <- readGeoJson filePath
    createMvt config geoJson

readGeoJson :: FilePath -> IO (Geospatial.GeoFeatureCollection Aeson.Value)
readGeoJson geoJsonFile = do
    bs <- ByteStringLazy.readFile geoJsonFile
    let ebs         = Aeson.eitherDecode' bs :: Either String (Geospatial.GeoFeatureCollection Aeson.Value)
        decodeError = error . (("Unable to decode " <> geoJsonFile <> ": ") <>)
    pure (either decodeError id ebs)

readMvt :: FilePath -> IO VectorTile.VectorTile
readMvt filePath = do
    b <- ByteString.readFile filePath
    let t = VectorTile.tile b
        rawDecodeError a = error ("Unable to read " <> filePath <> ": " <> Text.unpack a)
    pure (either rawDecodeError id t)

-- Lib

encodeMvt :: VectorTile.VectorTile -> ByteStringChar8.ByteString
encodeMvt = VectorTile.untile

createMvt :: TypesConfig.Config -> Geospatial.GeoFeatureCollection Aeson.Value -> IO VectorTile.VectorTile
createMvt TypesConfig.Config{..} (Geospatial.GeoFeatureCollection geoFeatureBbox geoFeatures) = do
  let sphericalMercatorPts = SphericalMercator.convertFeatures _extents _quantizePixels (SphericalMercator.boundingBox _gtc) geoFeatures
      clipBb = Clip.createBoundingBox _buffer _extents
      clippedFeatures = Clip.clipFeatures clipBb sphericalMercatorPts
      simplifiedFeatures = Simplify.simplifyFeatures _simplify clippedFeatures
      TypesGeoJsonFeatures.MvtFeatures{..} = ST.runST $ getFeatures (Geospatial.GeoFeatureCollection geoFeatureBbox simplifiedFeatures)
      layer = VectorTile.Layer (fromIntegral _version) _name mvtPoints mvtLines mvtPolygons (fromIntegral _extents)
  pure . VectorTile.VectorTile $ HashMapLazy.fromList [(_name, layer)]

getFeatures :: Geospatial.GeoFeatureCollection Aeson.Value -> ST.ST s TypesGeoJsonFeatures.MvtFeatures
getFeatures Geospatial.GeoFeatureCollection{..} = GeoJsonToMvt.geoJsonFeaturesToMvtFeatures TypesGeoJsonFeatures.emptyMvtFeatures _geofeatures

convertClipSimplify :: TypesConfig.Config -> Geospatial.GeospatialGeometry -> Geospatial.GeospatialGeometry
convertClipSimplify TypesConfig.Config{..} feature = simplifiedFeatures
  where
    sphericalMercatorPts = SphericalMercator.mapFeature _extents _quantizePixels (SphericalMercator.boundingBox _gtc) feature
    clipBb = Clip.createBoundingBox _buffer _extents
    clippedFeatures = Clip.mapFeature clipBb sphericalMercatorPts
    simplifiedFeatures = Simplify.mapFeature _simplify clippedFeatures
