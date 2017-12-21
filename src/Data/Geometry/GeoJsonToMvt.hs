{-# LANGUAGE FlexibleContexts #-}

module Data.Geometry.GeoJsonToMvt where

import qualified Control.Monad.ST                as ST
import qualified Data.Aeson                      as A
import qualified Data.Foldable                   as F (foldMap)
import qualified Data.Geospatial                 as GJ
import qualified Data.LinearRing                 as GJ
import qualified Data.LineString                 as GJ
import qualified Data.List                       as DL
import qualified Data.STRef                      as ST
import qualified Data.Text                       as T
import qualified Data.Vector                     as DV
import qualified Geography.VectorTile.Geometry   as VG

import           Data.Geometry.SphericalMercator
import           Data.Geometry.Types.MvtFeatures
import           Data.Geometry.Types.Types

-- Lib

geoJsonFeaturesToMvtFeatures :: (Pixels, BoundingBox) -> [GJ.GeoFeature A.Value] -> ST.ST s MvtFeatures
geoJsonFeaturesToMvtFeatures extentsBb features = do
  ops <- ST.newSTRef 0
  F.foldMap (convertFeature extentsBb ops) features

-- Feature

convertFeature :: (Pixels, BoundingBox) -> ST.STRef s Int -> GJ.GeoFeature A.Value -> ST.ST s MvtFeatures
convertFeature config ops (GJ.GeoFeature _ geom props mfid) = do
  fid <- convertId mfid ops
  pure $ convertGeometry config fid props geom

-- Geometry

convertGeometry :: (Pixels, BoundingBox) -> Int -> A.Value -> GJ.GeospatialGeometry -> MvtFeatures
convertGeometry config fid props geom =
  case geom of
    GJ.NoGeometry     -> mempty
    GJ.Point g        -> mkPoint fid props . convertPoint config $ g
    GJ.MultiPoint g   -> mkPoint fid props . convertMultiPoint config $ g
    GJ.Line g         -> mkLineString fid props . convertLineString config $ g
    GJ.MultiLine g    -> mkLineString fid props . convertMultiLineString config $ g
    GJ.Polygon g      -> mkPolygon fid props . convertPolygon config $ g
    GJ.MultiPolygon g -> mkPolygon fid props . convertMultiPolygon config $ g
    GJ.Collection gs  -> F.foldMap (convertGeometry config fid props) gs

-- FeatureID

readFeatureID :: Maybe GJ.FeatureID -> Maybe Int
readFeatureID mfid = do
  fid <- mfid
  case reads (T.unpack fid) of
    [(val, "")] -> Just val
    _           -> Nothing

convertId :: Maybe GJ.FeatureID -> ST.STRef s Int -> ST.ST s Int
convertId mfid ops =
  case readFeatureID mfid of
    Just val -> pure val
    Nothing  -> do
      ST.modifySTRef ops (+1)
      ST.readSTRef ops

-- Points

convertPoint :: (Pixels, BoundingBox) -> GJ.GeoPoint -> DV.Vector VG.Point
convertPoint config = coordsToPoints config . GJ._unGeoPoint

convertMultiPoint :: (Pixels, BoundingBox) -> GJ.GeoMultiPoint -> DV.Vector VG.Point
convertMultiPoint config = F.foldMap (convertPoint config) . GJ.splitGeoMultiPoint

-- Lines

convertLineString :: (Pixels, BoundingBox) -> GJ.GeoLine -> DV.Vector VG.LineString
convertLineString config =
    DV.singleton .
    VG.LineString .
    DV.convert .
    F.foldMap (coordsToPoints config) .
    GJ.fromLineString .
    GJ._unGeoLine

convertMultiLineString :: (Pixels, BoundingBox) -> GJ.GeoMultiLine -> DV.Vector VG.LineString
convertMultiLineString config = F.foldMap (convertLineString config) . GJ.splitGeoMultiLine

-- Polygons

convertPolygon :: (Pixels, BoundingBox) -> GJ.GeoPolygon -> DV.Vector VG.Polygon
convertPolygon config poly = DV.singleton $
  case GJ._unGeoPolygon poly of
    []    -> VG.Polygon mempty mempty
    (h:t) ->
      case t of
        []   -> mkPoly h
        rest -> VG.Polygon (mkPolyPoints h) (mkPolys rest)
  where
    mkPolyPoints = DV.convert . F.foldMap (coordsToPoints config) . GJ.fromLinearRing
    mkPoly lring = VG.Polygon (mkPolyPoints lring) mempty
    mkPolys      = DL.foldl' (\acc lring -> DV.cons (mkPoly lring) acc) mempty

convertMultiPolygon :: (Pixels, BoundingBox) -> GJ.GeoMultiPolygon -> DV.Vector VG.Polygon
convertMultiPolygon config = F.foldMap (convertPolygon config) . GJ.splitGeoMultiPolygon

-- Helpers

createLines :: [a] -> DV.Vector (a, a)
createLines = DV.fromList . (zip <*> tail)

coordsToPoints :: (Pixels, BoundingBox) -> [Double] -> DV.Vector VG.Point
coordsToPoints _ []        = mempty
coordsToPoints _ [_]       = mempty
coordsToPoints (ext, bb) x = fmap (latLonToXYInTile ext bb . uncurry LatLon) (createLines x)
