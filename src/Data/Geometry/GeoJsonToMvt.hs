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

geoJsonFeaturesToMvtFeatures :: ZoomConfig -> [DG.GeoFeature A.Value] -> ST.ST s MvtFeatures
geoJsonFeaturesToMvtFeatures zconfig features = do
  ops <- ST.newSTRef 0
  F.foldMap (convertFeature zconfig ops) features

-- Feature

convertFeature :: ZoomConfig -> ST.STRef s Int -> DG.GeoFeature A.Value -> ST.ST s MvtFeatures
convertFeature zconfig ops (DG.GeoFeature _ geom props mfid) = do
  fid <- convertId mfid ops
  pure $ convertGeometry zconfig fid props geom

-- Geometry

convertGeometry :: ZoomConfig -> Int -> A.Value -> DG.GeospatialGeometry -> MvtFeatures
convertGeometry zconfig fid props geom =
  case geom of
    DG.NoGeometry     -> mempty
    DG.Point g        -> mkPoint fid props . convertPoint zconfig $ g
    DG.MultiPoint g   -> mkPoint fid props . convertMultiPoint zconfig $ g
    DG.Line g         -> mkLineString fid props . convertLineString zconfig $ g
    DG.MultiLine g    -> mkLineString fid props . convertMultiLineString zconfig $ g
    DG.Polygon g      -> mkPolygon fid props . convertPolygon zconfig $ g
    DG.MultiPolygon g -> mkPolygon fid props . convertMultiPolygon zconfig $ g
    DG.Collection gs  -> F.foldMap (convertGeometry zconfig fid props) gs

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

convertPoint :: ZoomConfig -> DG.GeoPoint -> DV.Vector VG.Point
convertPoint zconfig = coordsToPoints zconfig . DG._unGeoPoint

convertMultiPoint :: ZoomConfig -> DG.GeoMultiPoint -> DV.Vector VG.Point
convertMultiPoint zconfig = F.foldMap (convertPoint zconfig) . DG.splitGeoMultiPoint

-- Lines

convertLineString :: ZoomConfig -> DG.GeoLine -> DV.Vector VG.LineString
convertLineString zconfig =
    DV.singleton .
    VG.LineString .
    DV.convert .
    F.foldMap (coordsToPoints zconfig) .
    DG.fromLineString .
    DG._unGeoLine

convertMultiLineString :: ZoomConfig -> DG.GeoMultiLine -> DV.Vector VG.LineString
convertMultiLineString zconfig = F.foldMap (convertLineString zconfig) . DG.splitGeoMultiLine

-- Polygons

convertPolygon :: ZoomConfig -> DG.GeoPolygon -> DV.Vector VG.Polygon
convertPolygon zconfig poly = DV.singleton $
  case DG._unGeoPolygon poly of
    []    -> VG.Polygon mempty mempty
    (h:t) ->
      case t of
        []   -> mkPoly h
        rest -> VG.Polygon (mkPolyPoints h) (mkPolys rest)
  where
    mkPolyPoints = DV.convert . F.foldMap (coordsToPoints zconfig) . DG.fromLinearRing
    mkPoly lring = VG.Polygon (mkPolyPoints lring) mempty
    mkPolys      = DL.foldl' (\acc lring -> DV.cons (mkPoly lring) acc) mempty

convertMultiPolygon :: ZoomConfig -> DG.GeoMultiPolygon -> DV.Vector VG.Polygon
convertMultiPolygon zconfig = F.foldMap (convertPolygon zconfig) . DG.splitGeoMultiPolygon

-- Helpers

createLines :: [a] -> DV.Vector (a, a)
createLines = DV.fromList . (zip <*> tail)

coordsToPoints :: ZoomConfig -> [Double] -> DV.Vector VG.Point
coordsToPoints _ []        = mempty
coordsToPoints _ [_]       = mempty
coordsToPoints (ZoomConfig ext q bb) x = fmap (latLonToXYInTile ext q bb . uncurry LatLon) (createLines x)
