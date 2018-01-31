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
import qualified Data.Sequence                   as DS
import qualified Data.STRef                      as ST
import qualified Data.Vector                     as DV
import qualified Data.Vector.Unboxed             as DVU
import qualified Geography.VectorTile            as VG

import qualified Data.Geometry.SphericalMercator as DGSM
import qualified Data.Geometry.Types.MvtFeatures as DGTMF
import qualified Data.Geometry.Types.Simplify    as DGTS
import qualified Data.Geometry.Types.Types       as DGTT

-- Lib

geoJsonFeaturesToMvtFeatures :: DGTT.ZoomConfig -> [DG.GeoFeature A.Value] -> ST.ST s DGTMF.MvtFeatures
geoJsonFeaturesToMvtFeatures zConfig features = do
  ops <- ST.newSTRef 0
  F.foldMap (convertFeature zConfig ops) features

-- Feature

convertFeature :: DGTT.ZoomConfig -> ST.STRef s Word -> DG.GeoFeature A.Value -> ST.ST s DGTMF.MvtFeatures
convertFeature zConfig ops (DG.GeoFeature _ geom props mfid) = do
  fid <- convertId mfid ops
  pure $ convertGeometry zConfig fid props geom

-- Geometry

convertGeometry :: DGTT.ZoomConfig -> Word -> A.Value -> DG.GeospatialGeometry -> DGTMF.MvtFeatures
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

convertPoint :: DGTT.ZoomConfig -> DG.GeoPoint -> DS.Seq VG.Point
convertPoint zConfig = coordsToPoints' zConfig . DG._unGeoPoint

convertMultiPoint :: DGTT.ZoomConfig -> DG.GeoMultiPoint -> DS.Seq VG.Point
convertMultiPoint zConfig = F.foldMap (convertPoint zConfig) . DG.splitGeoMultiPoint

-- Lines

convertLineString :: DGTT.ZoomConfig -> DG.GeoLine -> DS.Seq VG.LineString
convertLineString zConfig@DGTT.ZoomConfig{..} =
    DS.singleton . VG.LineString .
    ((DGTS.simplifUsing _zcSimplify) . DV.convert . F.foldMap (coordsToPoints zConfig) . DG.fromLineString . DG._unGeoLine)

convertMultiLineString :: DGTT.ZoomConfig -> DG.GeoMultiLine -> DS.Seq VG.LineString
convertMultiLineString zConfig = F.foldMap (convertLineString zConfig) . DG.splitGeoMultiLine

-- Polygons

convertPolygon :: DGTT.ZoomConfig -> DG.GeoPolygon -> DS.Seq VG.Polygon
convertPolygon zConfig poly = DS.singleton $
  case DG._unGeoPolygon poly of
    []    -> VG.Polygon mempty mempty
    (h:t) ->
      case t of
        []   -> mkPoly zConfig h
        rest -> VG.Polygon (mkPolyPoints zConfig h) (mkPolys zConfig rest)

mkPolys :: Foldable t => DGTT.ZoomConfig -> t (DG.LinearRing [Double]) -> DS.Seq VG.Polygon
mkPolys zConfig = DL.foldl' (\acc lring -> (mkPoly zConfig lring DS.<| acc)) DS.empty

mkPoly :: DGTT.ZoomConfig -> DG.LinearRing [Double] -> VG.Polygon
mkPoly zConfig lring = VG.Polygon (mkPolyPoints zConfig lring) mempty

mkPolyPoints :: DGTT.ZoomConfig -> DG.LinearRing [Double] -> DVU.Vector VG.Point
mkPolyPoints zConfig@DGTT.ZoomConfig{..} = (DGTS.simplifUsing _zcSimplify) . DV.convert . F.foldMap (coordsToPoints zConfig) . DG.fromLinearRing

convertMultiPolygon :: DGTT.ZoomConfig -> DG.GeoMultiPolygon -> DS.Seq VG.Polygon
convertMultiPolygon zConfig = F.foldMap (convertPolygon zConfig) . DG.splitGeoMultiPolygon

-- Helpers

createLines :: [a] -> DV.Vector (a, a)
createLines = DV.fromList . (zip <*> tail)

createLines' :: [a] -> DS.Seq (a, a)
createLines' = DS.fromList . (zip <*> tail)

coordsToPoints :: DGTT.ZoomConfig -> [Double] -> DV.Vector VG.Point
coordsToPoints _ []        = mempty
coordsToPoints _ [_]       = mempty
coordsToPoints (DGTT.ZoomConfig ext q bb _) x = fmap (DGSM.latLonToXYInTile ext q bb . uncurry DGTT.LatLon) (createLines x)

coordsToPoints' :: DGTT.ZoomConfig -> [Double] -> DS.Seq VG.Point
coordsToPoints' _ []        = mempty
coordsToPoints' _ [_]       = mempty
coordsToPoints' (DGTT.ZoomConfig ext q bb _) x = fmap (DGSM.latLonToXYInTile ext q bb . uncurry DGTT.LatLon) (createLines' x)
