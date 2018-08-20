{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Data.Geometry.GeoJsonToMvt where

import qualified Control.Monad.ST                as ST
import qualified Data.Aeson                      as A
import qualified Data.Foldable                   as F (foldMap)
import qualified Data.Geospatial                 as DG
import qualified Data.LinearRing                 as DG
import qualified Data.LineString                 as DG
import qualified Data.List                       as DL
import qualified Data.STRef                      as ST
import qualified Data.Vector                     as DataVector
import qualified Data.Vector.Storable            as DataVectorStorable
import qualified Geography.VectorTile            as VG
import qualified Geography.VectorTile            as GeographyVectorTile

import qualified Data.Geometry.SphericalMercator as DGSM
import qualified Data.Geometry.Types.Config      as TypesConfig
import qualified Data.Geometry.Types.Geography   as TypesGeography
import qualified Data.Geometry.Types.MvtFeatures as DGTMF
import qualified Data.Geometry.Types.Simplify    as DGTS

-- Lib

geoJsonFeaturesToMvtFeatures :: TypesConfig.ZoomConfig -> [DG.GeoFeature A.Value] -> ST.ST s DGTMF.MvtFeatures
geoJsonFeaturesToMvtFeatures zConfig features = do
  ops <- ST.newSTRef 0
  F.foldMap (convertFeature zConfig ops) features

-- Feature

convertFeature :: TypesConfig.ZoomConfig -> ST.STRef s Word -> DG.GeoFeature A.Value -> ST.ST s DGTMF.MvtFeatures
convertFeature zConfig ops (DG.GeoFeature _ geom props mfid) = do
  fid <- convertId mfid ops
  pure $ convertGeometry zConfig fid props geom

-- Geometry

convertGeometry :: TypesConfig.ZoomConfig -> Word -> A.Value -> DG.GeospatialGeometry -> DGTMF.MvtFeatures
convertGeometry zConfig fid props geom =
  case geom of
    DG.NoGeometry     -> mempty
    DG.Point g        -> DGTMF.mkPoint fid props . convertPoint zConfig $ g
    DG.MultiPoint g   -> DGTMF.mkPoint fid props . convertMultiPoint zConfig $ g
    DG.Line g         -> DGTMF.mkLineString fid props . convertLineString zConfig $ g
    DG.MultiLine g    -> DGTMF.mkLineString fid props . convertMultiLineString zConfig $ g
    DG.Polygon g      -> DGTMF.mkPolygon fid props . convertPolygon zConfig $ g
    DG.MultiPolygon g -> DGTMF.mkPolygon fid props . convertMultiPolygon zConfig $ g
    DG.Collection gs  -> F.foldMap (convertGeometry zConfig fid props) gs

-- FeatureID

readFeatureID :: Maybe DG.FeatureID -> Maybe Word
readFeatureID mfid =
  case mfid of
    Just (DG.FeatureIDNumber x) -> Just (fromIntegral x)
    _                           -> Nothing

convertId :: Maybe DG.FeatureID -> ST.STRef s Word -> ST.ST s Word
convertId mfid ops =
  case readFeatureID mfid of
    Just val -> pure val
    Nothing  -> do
      ST.modifySTRef ops (+1)
      ST.readSTRef ops

-- Points

convertPoint :: TypesConfig.ZoomConfig -> DG.GeoPoint -> DataVectorStorable.Vector GeographyVectorTile.Point
convertPoint zConfig = coordsToPoints' zConfig . DG._unGeoPoint

convertMultiPoint :: TypesConfig.ZoomConfig -> DG.GeoMultiPoint -> DataVectorStorable.Vector GeographyVectorTile.Point
convertMultiPoint zConfig = F.foldMap (convertPoint zConfig) . DG.splitGeoMultiPoint

-- Lines

convertLineString :: TypesConfig.ZoomConfig -> DG.GeoLine -> DataVector.Vector GeographyVectorTile.LineString
convertLineString zConfig@TypesConfig.ZoomConfig{..} =
  DataVector.singleton .
  VG.LineString .
  DGTS.simplifyUsing _zcSimplify .
  DataVector.convert .
  F.foldMap (coordsToPoints zConfig) .
  DG.fromLineString .
  DG._unGeoLine

convertMultiLineString :: TypesConfig.ZoomConfig -> DG.GeoMultiLine -> DataVector.Vector GeographyVectorTile.LineString
convertMultiLineString zConfig = F.foldMap (convertLineString zConfig) . DG.splitGeoMultiLine

-- Polygons

convertPolygon :: TypesConfig.ZoomConfig -> DG.GeoPolygon -> DataVector.Vector VG.Polygon
convertPolygon zConfig poly = DataVector.singleton $
  case DG._unGeoPolygon poly of
    []    -> VG.Polygon mempty mempty
    (h:t) ->
      case t of
        []   -> mkPoly zConfig h
        rest -> VG.Polygon (mkPolyPoints zConfig h) (mkPolys zConfig rest)

mkPolys :: Foldable t => TypesConfig.ZoomConfig -> t (DG.LinearRing [Double]) -> DataVector.Vector VG.Polygon
mkPolys zConfig = DL.foldl' (\acc lring -> (mkPoly zConfig lring `DataVector.cons` acc)) DataVector.empty

mkPoly :: TypesConfig.ZoomConfig -> DG.LinearRing [Double] -> VG.Polygon
mkPoly zConfig lring = VG.Polygon (mkPolyPoints zConfig lring) mempty

mkPolyPoints :: TypesConfig.ZoomConfig -> DG.LinearRing [Double] -> DataVectorStorable.Vector VG.Point
mkPolyPoints zConfig@TypesConfig.ZoomConfig{..} = DGTS.simplifyUsing _zcSimplify . DataVector.convert . F.foldMap (coordsToPoints zConfig) . DG.fromLinearRing

convertMultiPolygon :: TypesConfig.ZoomConfig -> DG.GeoMultiPolygon -> DataVector.Vector VG.Polygon
convertMultiPolygon zConfig = F.foldMap (convertPolygon zConfig) . DG.splitGeoMultiPolygon

-- Helpers

createLines :: [a] -> DataVector.Vector (a, a)
createLines = DataVector.fromList . (zip <*> tail)

createLines' :: [a] -> [(a, a)]
createLines' = zip <*> tail

coordsToPoints :: TypesConfig.ZoomConfig -> [Double] -> DataVector.Vector GeographyVectorTile.Point
coordsToPoints _ []        = mempty
coordsToPoints _ [_]       = mempty
coordsToPoints (TypesConfig.ZoomConfig ext q bb _) x = fmap (DGSM.latLonToXYInTile ext q bb . uncurry TypesGeography.LatLon) (createLines x)

coordsToPoints' :: TypesConfig.ZoomConfig -> [Double] -> DataVectorStorable.Vector GeographyVectorTile.Point
coordsToPoints' _ []        = mempty
coordsToPoints' _ [_]       = mempty
coordsToPoints' (TypesConfig.ZoomConfig ext q bb _) coords = DL.foldl' (\a (lat, lon) -> DataVectorStorable.snoc a $ DGSM.latLonToXYInTile ext q bb (TypesGeography.LatLon lat lon)) DataVectorStorable.empty (createLines' coords)

