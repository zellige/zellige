{-# LANGUAGE FlexibleContexts #-}

module Data.Geometry.GeoJsonToMvt where

import qualified Control.Monad.ST                as ST
import qualified Data.Aeson                      as A
import qualified Data.Foldable                   as F (foldMap)
import qualified Data.Geography.GeoJSON          as GJ
import qualified Data.List                       as DL
import qualified Data.Scientific                 as S
import qualified Data.STRef                      as ST
import qualified Data.Vector                     as DV
import qualified Geography.VectorTile.Geometry   as VG

import           Data.Geometry.SphericalMercator
import           Data.Geometry.Types.MvtFeatures
import           Data.Geometry.Types.Types

geoJsonFeaturesToMvtFeatures :: (Pixels, BoundingBox) -> [GJ.Feature] -> ST.ST s MvtFeatures
geoJsonFeaturesToMvtFeatures extentsBb features = do
  ops <- ST.newSTRef 0
  F.foldMap (convertFeature extentsBb ops) features

convertFeature :: (Pixels, BoundingBox) -> ST.STRef s Int -> GJ.Feature -> ST.ST s MvtFeatures
convertFeature config ops (GJ.Feature _ geom props fid) = do
  x <- convertId fid ops
  pure $ go x geom
  where
      go x (GJ.Point p)                  = mkPoint x props . convertPoint config $ p
      go x (GJ.MultiPoint mpg)           = mkPoint x props . convertMultiPoint config $ mpg
      go x (GJ.LineString ls)            = mkLineString x props . convertLineString config $ ls
      go x (GJ.MultiLineString mls)      = mkLineString x props . convertMultiLineString config $ mls
      go x (GJ.Polygon poly)             = mkPolygon x props . convertPolygon config $ poly
      go x (GJ.MultiPolygon mp)          = mkPolygon x props . convertMultiPolygon config $ mp
      go x (GJ.GeometryCollection geoms) = F.foldMap (go x) geoms


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

convertId :: Maybe A.Value -> ST.STRef s Int -> ST.ST s Int
convertId (Just (A.Number n)) _ = pure $ (round . sToF) n
convertId _                   ops = do
  ST.modifySTRef ops (+1)
  ST.readSTRef ops

pointToMvt :: (Pixels, BoundingBox) -> [GJ.PointGeometry] -> DV.Vector VG.Point
pointToMvt config = F.foldMap (sciLatLongToPoints config . GJ.coordinates)

lineToMvt :: (Pixels, BoundingBox) -> [GJ.LineStringGeometry] -> DV.Vector VG.LineString
lineToMvt config = DL.foldl' (\acc lsg -> DV.cons (createLineString lsg) acc) DV.empty
    where
      createLineString lsg = VG.LineString (getPoints lsg)
      getPoints lsg = DV.convert $ pointToMvt config $ GJ.lineString lsg

polygonToMvt :: (Pixels, BoundingBox) -> [GJ.PolygonGeometry] -> DV.Vector VG.Polygon
polygonToMvt config = DL.foldl' (\acc poly -> DV.cons (mkNewPolygon poly) acc) DV.empty
  where
    mkNewPolygon poly = VG.Polygon (ext (GJ.exterior poly)) (int (GJ.holes poly))
    ext p = DV.convert $ pointToMvt config p
    int p = polygonToMvt config $ fmap (\x -> GJ.PolygonGeometry x []) p

sciLatLongToPoints :: (Pixels, BoundingBox) -> [S.Scientific] -> DV.Vector VG.Point
sciLatLongToPoints _ []        = DV.empty
sciLatLongToPoints _ [_]       = DV.empty
sciLatLongToPoints (ext, bb) x = DV.map (\(lat, lon) -> latLonToXYInTile ext bb (LatLon (sToF lat) (sToF lon))) (createLines x)

createLines :: [a] -> DV.Vector (a, a)
createLines a = DV.fromList $ (zip <*> tail) a
