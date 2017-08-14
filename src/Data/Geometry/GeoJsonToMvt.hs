{-# LANGUAGE FlexibleContexts #-}

module Data.Geometry.GeoJsonToMvt where

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

geoJsonFeaturesToMvtFeatures :: (Pixels, BoundingBox) -> [GJ.Feature] -> (DV.Vector (VT.Feature VG.Point), DV.Vector (VT.Feature VG.LineString), DV.Vector (VT.Feature VG.Polygon))
geoJsonFeaturesToMvtFeatures extentsBb = F.foldMap (convertFeature extentsBb)

convertFeature :: (Pixels, BoundingBox) -> GJ.Feature -> (DV.Vector (VT.Feature VG.Point), DV.Vector (VT.Feature VG.LineString), DV.Vector (VT.Feature VG.Polygon))
convertFeature config (GJ.Feature _ geom props fid) = go geom
  where
      go (GJ.Point p)                  = mkPoint . convertPoint config $ p
      go (GJ.MultiPoint mpg)           = mkPoint . convertMultiPoint config $ mpg
      go (GJ.LineString ls)            = mkLineString . convertLineString config $ ls
      go (GJ.MultiLineString mls)      = mkLineString . convertMultiLineString config $ mls
      go (GJ.Polygon poly)             = mkPolygon . convertPolygon config $ poly
      go (GJ.MultiPolygon mp)          = mkPolygon . convertMultiPolygon config $ mp
      go (GJ.GeometryCollection geoms) = F.foldMap go geoms
      mkPoint p       = (mkFeature p, mempty, mempty)
      mkLineString l  = (mempty, mkFeature l, mempty)
      mkPolygon o     = (mempty, mempty, mkFeature o)
      mkFeature geoms = DV.singleton $ VT.Feature (convertId fid) (convertProps props) geoms

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

convertId :: Maybe A.Value -> Int
convertId (Just (A.Number n)) = (round . sToF) n
convertId _                   = 0

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
