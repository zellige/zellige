{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.Geometry.MapnikVectorTile where

import qualified Control.Monad.ST                as ST
import qualified Data.Aeson                      as Aeson
import qualified Data.ByteString                 as ByteString
import qualified Data.ByteString.Char8           as ByteStringChar8
import qualified Data.ByteString.Lazy            as ByteStringLazy
import qualified Data.Geospatial                 as Geospatial
import qualified Data.HashMap.Lazy               as HashMapLazy
import           Data.Monoid                     ((<>))
import qualified Data.Text                       as Text
import qualified Data.Vector                     as Vector
import qualified Geography.VectorTile            as VectorTile

import qualified Data.Geometry.Clip              as Clip
import qualified Data.Geometry.GeoJsonToMvt      as GeoJsonToMvt
import qualified Data.Geometry.Types.Config      as Config
import qualified Data.Geometry.Types.Geography   as Geography
import qualified Data.Geometry.Types.LayerConfig as LayerConfig
import qualified Data.Geometry.Types.MvtFeatures as MvtFeatures


-- Command line

writeLayer :: LayerConfig.LayerConfig -> IO ()
writeLayer lc = do
    mvt <- geoJsonFileToMvt (LayerConfig._layerInput lc) (configFromLayerConfig lc)
    ByteString.writeFile (LayerConfig._layerOutput lc) (encodeMvt mvt)

configFromLayerConfig :: LayerConfig.LayerConfig -> Config.Config
configFromLayerConfig LayerConfig.LayerConfig{..}  = Config.mkConfig _layerName _layerZoom (_layerX, _layerY) _layerBuffer _layerExtent _layerQuantizePixels _layerSimplification

geoJsonFileToMvt :: FilePath -> Config.Config -> IO VectorTile.VectorTile
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

createMvt :: Config.Config -> Geospatial.GeoFeatureCollection Aeson.Value -> IO VectorTile.VectorTile
createMvt Config.Config{..} geoJson = do
    let MvtFeatures.MvtFeatures{..} = ST.runST $ getFeatures geoJson
        -- zConfig = Config.ZoomConfig _extents _quantizePixels (SphericalMercator.boundingBox _gtc) _simplify
        -- clipBb = Clip.createBoundingBox _buffer _extents
        layer = VectorTile.Layer (fromIntegral _version) _name mvtPoints mvtLines mvtPolygons (fromIntegral _extents)
    pure . VectorTile.VectorTile $ HashMapLazy.fromList [(_name, layer)]

getFeatures :: Geospatial.GeoFeatureCollection Aeson.Value -> ST.ST s MvtFeatures.MvtFeatures
getFeatures Geospatial.GeoFeatureCollection{..} = GeoJsonToMvt.geoJsonFeaturesToMvtFeatures MvtFeatures.emptyMvtFeatures _geofeatures

clipFeatures :: Geography.BoundingBox -> Vector.Vector (Geospatial.GeoFeature Aeson.Value) -> Vector.Vector (Geospatial.GeoFeature Aeson.Value)
clipFeatures bbox = Vector.foldr (clipFeature bbox) Vector.empty

clipFeature :: Geography.BoundingBox -> Geospatial.GeoFeature Aeson.Value -> Vector.Vector (Geospatial.GeoFeature Aeson.Value) -> Vector.Vector (Geospatial.GeoFeature Aeson.Value)
clipFeature bbox feature@Geospatial.GeoFeature{..} acc =
    case _geometry of
        Geospatial.NoGeometry     -> acc
        Geospatial.Point g        -> Clip.clipPoint bbox g feature acc
        Geospatial.MultiPoint g   -> Clip.clipPoints bbox g feature acc
        Geospatial.Line g         -> Clip.newClipLineCs bbox g feature acc
        Geospatial.MultiLine g    -> Clip.newClipLinesCs bbox g feature acc
        Geospatial.Polygon _      -> acc
        Geospatial.MultiPolygon _ -> acc
        Geospatial.Collection _   -> acc

