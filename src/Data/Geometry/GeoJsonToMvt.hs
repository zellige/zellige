{-# LANGUAGE FlexibleContexts #-}

module Data.Geometry.GeoJsonToMvt where

import qualified Control.Monad.ST                as ST
import qualified Data.Aeson                      as A
import qualified Data.Foldable                   as F (foldMap)
import qualified Data.Geospatial                 as DG
import qualified Data.LinearRing                 as DG
import qualified Data.LineString                 as DG
import qualified Data.List                       as DL
import qualified Data.STRef                      as ST
import qualified Data.Vector                     as DV
import qualified Geography.VectorTile.Geometry   as VG

import           Data.Geometry.SphericalMercator
import           Data.Geometry.Types.MvtFeatures
import           Data.Geometry.Types.Types

-- Lib

geoJsonFeaturesToMvtFeatures :: (Pixels, BoundingBox) -> [DG.GeoFeature A.Value] -> ST.ST s MvtFeatures
geoJsonFeaturesToMvtFeatures extentsBb features = do
  ops <- ST.newSTRef 0
  F.foldMap (convertFeature extentsBb ops) features

-- Feature

convertFeature :: (Pixels, BoundingBox) -> ST.STRef s Int -> DG.GeoFeature A.Value -> ST.ST s MvtFeatures
convertFeature config ops (DG.GeoFeature _ geom props mfid) = do
  fid <- convertId mfid ops
  pure $ convertGeometry config fid props geom

-- Geometry

convertGeometry :: (Pixels, BoundingBox) -> Int -> A.Value -> DG.GeospatialGeometry -> MvtFeatures
convertGeometry config fid props geom =
  case geom of
    DG.NoGeometry     -> mempty
    DG.Point g        -> mkPoint fid props . convertPoint config $ g
    DG.MultiPoint g   -> mkPoint fid props . convertMultiPoint config $ g
    DG.Line g         -> mkLineString fid props . convertLineString config $ g
    DG.MultiLine g    -> mkLineString fid props . convertMultiLineString config $ g
    DG.Polygon g      -> mkPolygon fid props . convertPolygon config $ g
    DG.MultiPolygon g -> mkPolygon fid props . convertMultiPolygon config $ g
    DG.Collection gs  -> F.foldMap (convertGeometry config fid props) gs

-- FeatureID

readFeatureID :: Maybe DG.FeatureID -> Maybe Int
readFeatureID mfid =
  case mfid of
    Just (DG.FeatureIDNumber x) -> Just x
    _                           -> Nothing

convertId :: Maybe DG.FeatureID -> ST.STRef s Int -> ST.ST s Int
convertId mfid ops =
  case readFeatureID mfid of
    Just val -> pure val
    Nothing  -> do
      ST.modifySTRef ops (+1)
      ST.readSTRef ops

-- Points

convertPoint :: (Pixels, BoundingBox) -> DG.GeoPoint -> DV.Vector VG.Point
convertPoint config = coordsToPoints config . DG._unGeoPoint

convertMultiPoint :: (Pixels, BoundingBox) -> DG.GeoMultiPoint -> DV.Vector VG.Point
convertMultiPoint config = F.foldMap (convertPoint config) . DG.splitGeoMultiPoint

-- Lines

convertLineString :: (Pixels, BoundingBox) -> DG.GeoLine -> DV.Vector VG.LineString
convertLineString config =
    DV.singleton .
    VG.LineString .
    DV.convert .
    F.foldMap (coordsToPoints config) .
    DG.fromLineString .
    DG._unGeoLine

convertMultiLineString :: (Pixels, BoundingBox) -> DG.GeoMultiLine -> DV.Vector VG.LineString
convertMultiLineString config = F.foldMap (convertLineString config) . DG.splitGeoMultiLine

-- Polygons

convertPolygon :: (Pixels, BoundingBox) -> DG.GeoPolygon -> DV.Vector VG.Polygon
convertPolygon config poly = DV.singleton $
  case DG._unGeoPolygon poly of
    []    -> VG.Polygon mempty mempty
    (h:t) ->
      case t of
        []   -> mkPoly h
        rest -> VG.Polygon (mkPolyPoints h) (mkPolys rest)
  where
    mkPolyPoints = DV.convert . F.foldMap (coordsToPoints config) . DG.fromLinearRing
    mkPoly lring = VG.Polygon (mkPolyPoints lring) mempty
    mkPolys      = DL.foldl' (\acc lring -> DV.cons (mkPoly lring) acc) mempty

convertMultiPolygon :: (Pixels, BoundingBox) -> DG.GeoMultiPolygon -> DV.Vector VG.Polygon
convertMultiPolygon config = F.foldMap (convertPolygon config) . DG.splitGeoMultiPolygon

-- Helpers

createLines :: [a] -> DV.Vector (a, a)
createLines = DV.fromList . (zip <*> tail)

coordsToPoints :: (Pixels, BoundingBox) -> [Double] -> DV.Vector VG.Point
coordsToPoints _ []        = mempty
coordsToPoints _ [_]       = mempty
coordsToPoints (ext, bb) x = fmap (latLonToXYInTile ext bb . uncurry LatLon) (createLines x)
