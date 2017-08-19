{-# LANGUAGE FlexibleContexts #-}

module Data.Geometry.GeoJsonToMvt where

import qualified Control.Concurrent.STM          as CS
import qualified Data.Aeson                      as A
import qualified Data.Foldable                   as F (foldMap)
import qualified Data.Geography.GeoJSON          as GJ
import qualified Data.HashMap.Strict             as HM
import qualified Data.List                       as DL
import qualified Data.Map.Lazy                   as DMZ
import           Data.Maybe
import           Data.Scientific
import qualified Data.Text                       as T (Text)
import qualified Data.Vector                     as DV
import qualified Geography.VectorTile.Geometry   as VG
import qualified Geography.VectorTile.VectorTile as VT

import           Data.Geometry.SphericalMercator
import           Data.Geometry.Types

geoJsonFeaturesToMvtFeatures :: (Pixels, BoundingBox) -> [GJ.Feature] -> IO (DV.Vector (VT.Feature VG.Point), DV.Vector (VT.Feature VG.LineString), DV.Vector (VT.Feature VG.Polygon))
geoJsonFeaturesToMvtFeatures extentsBb features = do
  ops <- CS.atomically $ CS.newTVar (0 :: Int)
  F.foldMap (convertFeature extentsBb ops) features

convertFeature :: (Pixels, BoundingBox) -> CS.TVar Int -> GJ.Feature -> IO (DV.Vector (VT.Feature VG.Point), DV.Vector (VT.Feature VG.LineString), DV.Vector (VT.Feature VG.Polygon))
convertFeature config ops (GJ.Feature _ geom props fid) = do
  x <- convertId fid ops
  pure $ go x geom
  where
      go x (GJ.Point p)                  = mkPoint x . convertPoint config $ p
      go x (GJ.MultiPoint mpg)           = mkPoint x . convertMultiPoint config $ mpg
      go x (GJ.LineString ls)            = mkLineString x . convertLineString config $ ls
      go x (GJ.MultiLineString mls)      = mkLineString x . convertMultiLineString config $ mls
      go x (GJ.Polygon poly)             = mkPolygon x . convertPolygon config $ poly
      go x (GJ.MultiPolygon mp)          = mkPolygon x . convertMultiPolygon config $ mp
      go x (GJ.GeometryCollection geoms) = F.foldMap (go x) geoms
      mkPoint x p       = (mkFeature x p, mempty, mempty)
      mkLineString x l  = (mempty, mkFeature x l, mempty)
      mkPolygon x o     = (mempty, mempty, mkFeature x o)
      mkFeature x geoms = DV.singleton $ VT.Feature x (convertProps props) geoms

convertPoint :: (Pixels, BoundingBox) -> GJ.PointGeometry -> DV.Vector VG.Point
convertPoint config = sciLatLongToPoints config . GJ.coordinates

convertMultiPoint :: (Pixels, BoundingBox) -> GJ.MultiPointGeometry -> DV.Vector VG.Point
convertMultiPoint config = pointToMvt config . GJ.points

convertLineString :: (Pixels, BoundingBox) -> GJ.LineStringGeometry -> DV.Vector VG.LineString
convertLineString config = lineToMvt config . pure

convertMultiLineString :: (Pixels, BoundingBox) -> GJ.MultiLineStringGeometry -> DV.Vector VG.LineString
convertMultiLineString config (GJ.MultiLineStringGeometry mls) = lineToMvt config mls

convertPolygon :: (Pixels, BoundingBox) -> GJ.PolygonGeometry -> DV.Vector VG.Polygon
convertPolygon config = polygonToMvt config . pure

convertMultiPolygon :: (Pixels, BoundingBox) -> GJ.MultiPolygonGeometry -> DV.Vector VG.Polygon
convertMultiPolygon config (GJ.MultiPolygonGeometry polys) = polygonToMvt config polys

convertProps :: A.Value -> DMZ.Map T.Text VT.Val
convertProps (A.Object x) = DMZ.fromList . catMaybes $ Prelude.fmap convertElems (HM.toList x)
convertProps _ = DMZ.empty

convertId :: Maybe A.Value -> CS.TVar Int -> IO Int
convertId (Just (A.Number n)) _ = pure $ (round . sToF) n
convertId _                   ops = do
  CS.atomically $ CS.modifyTVar ops (+1)
  x <- CS.atomically $ CS.readTVar ops
  pure x

pointToMvt :: (Pixels, BoundingBox) -> [GJ.PointGeometry] -> DV.Vector VG.Point
pointToMvt config = F.foldMap (sciLatLongToPoints config . GJ.coordinates)

lineToMvt :: (Pixels, BoundingBox) -> [GJ.LineStringGeometry] -> DV.Vector VG.LineString
lineToMvt config = DL.foldl' (\acc lsg -> DV.cons (createLineString lsg) acc) DV.empty
    where
      createLineString lsg = VG.LineString (getPoints lsg)
      getPoints lsg = DV.convert $ pointToMvt config $ GJ.lineString lsg

polygonToMvt :: (Pixels, BoundingBox) -> [GJ.PolygonGeometry] -> DV.Vector VG.Polygon
polygonToMvt config = DL.foldl' (\acc poly -> DV.cons (mkPolygon poly) acc) DV.empty
  where
    mkPolygon poly = VG.Polygon (ext (GJ.exterior poly)) (int (GJ.holes poly))
    ext p = DV.convert $ pointToMvt config p
    int p = polygonToMvt config $ fmap (\x -> GJ.PolygonGeometry x []) p

sToF :: Scientific -> Double
sToF = toRealFloat

convertElems :: (t, A.Value) -> Maybe (t, VT.Val)
convertElems (k, A.String v) = Just (k, VT.St v)
convertElems (k, A.Number v) = Just (k, VT.Do (sToF v))
convertElems (k, A.Bool v)   = Just (k, VT.B v)
convertElems _               = Nothing

sciLatLongToPoints :: (Pixels, BoundingBox) -> [Scientific] -> DV.Vector VG.Point
sciLatLongToPoints _ []        = DV.empty
sciLatLongToPoints _ [_]       = DV.empty
sciLatLongToPoints (ext, bb) x = DV.map (\(lat, lon) -> latLonToXYInTile ext bb (LatLon (sToF lat) (sToF lon))) (createLines x)

createLines :: [a] -> DV.Vector (a, a)
createLines a = DV.fromList $ (zip <*> tail) a
