module Data.Geometry.GeoJsonToMvt where

import qualified Data.Aeson                      as A
import qualified Data.Foldable                   as F (foldMap)
import qualified Data.Geography.GeoJSON          as GJ
import qualified Data.HashMap.Strict             as HM
import qualified Data.Map.Lazy                   as DMZ
import           Data.Maybe
import           Data.Scientific
import qualified Data.Text                       as T (Text)
import qualified Data.Vector                     as DV
import qualified Data.Vector.Unboxed             as DVU
import qualified Geography.VectorTile.Geometry   as VG
import qualified Geography.VectorTile.VectorTile as VT

import           Data.Geometry.SphericalMercator
import           Data.Geometry.Types

geoJsonFeaturesToMvtFeatures :: Config -> [GJ.Feature] -> (DV.Vector (VT.Feature VG.Point), DV.Vector (VT.Feature VG.LineString), DV.Vector (VT.Feature VG.Polygon))
geoJsonFeaturesToMvtFeatures config = F.foldMap (convertFeature extentsBb)
  where
    extentsBb = (_extents config, boundingBox $ _gtc config)

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
      mkFeature geoms = DV.singleton $ VT.Feature (convertId fid) (convertProps props) (DV.fromList geoms)

convertPoint :: (Pixels, BoundingBox) -> GJ.PointGeometry -> [VG.Point]
convertPoint config = sciLatLongToPoints config . GJ.coordinates

convertMultiPoint :: (Pixels, BoundingBox) -> GJ.MultiPointGeometry -> [VG.Point]
convertMultiPoint config = pointToMvt config . GJ.points

convertLineString :: (Pixels, BoundingBox) -> GJ.LineStringGeometry -> [VG.LineString]
convertLineString config = lineToMvt config . pure

convertMultiLineString :: (Pixels, BoundingBox) -> GJ.MultiLineStringGeometry -> [VG.LineString]
convertMultiLineString config (GJ.MultiLineStringGeometry mls) = lineToMvt config mls

convertPolygon :: (Pixels, BoundingBox) -> GJ.PolygonGeometry -> [VG.Polygon]
convertPolygon config = polygonToMvt config . pure

convertMultiPolygon :: (Pixels, BoundingBox) -> GJ.MultiPolygonGeometry -> [VG.Polygon]
convertMultiPolygon config (GJ.MultiPolygonGeometry polys) = polygonToMvt config polys

convertProps :: A.Value -> DMZ.Map T.Text VT.Val
convertProps (A.Object x) = DMZ.fromList . catMaybes $ Prelude.fmap convertElems (HM.toList x)
convertProps _ = DMZ.empty

convertId :: Maybe A.Value -> Int
convertId (Just (A.Number n)) = (round . sToF) n
convertId _ = 0

pointToMvt :: (Pixels, BoundingBox) -> [GJ.PointGeometry] -> [VG.Point]
pointToMvt config = F.foldMap (sciLatLongToPoints config . GJ.coordinates)

lineToMvt :: (Pixels, BoundingBox) -> [GJ.LineStringGeometry] -> [VG.LineString]
lineToMvt config = F.foldMap (\lsg -> [VG.LineString . DVU.fromList . pointToMvt config $ GJ.lineString lsg])

polygonToMvt :: (Pixels, BoundingBox) -> [GJ.PolygonGeometry] -> [VG.Polygon]
polygonToMvt config = F.foldMap (\poly -> [VG.Polygon (ext (GJ.exterior poly)) (int (GJ.holes poly))])
  where
    ext = DVU.fromList . pointToMvt config
    int y = DV.fromList . polygonToMvt config $ fmap (\x -> GJ.PolygonGeometry x []) y

sToF :: Scientific -> Double
sToF = toRealFloat

convertElems :: (t, A.Value) -> Maybe (t, VT.Val)
convertElems (k, A.String v) = Just (k, VT.St v)
convertElems (k, A.Number v) = Just (k, VT.Do (sToF v))
convertElems (k, A.Bool v) = Just (k, VT.B v)
convertElems _ = Nothing

sciLatLongToPoints :: (Pixels, BoundingBox) -> [Scientific] -> [VG.Point]
sciLatLongToPoints _ [] = []
sciLatLongToPoints _ [_] = []
sciLatLongToPoints (ext, bb) x = fmap (\(lat, lon) -> latLonToXYInTile ext bb (LatLon (sToF lat) (sToF lon))) (zip <*> tail $ x)

-- writeOut = do
--     _ <- BS.writeFile "/tmp/out.mvt" (V.encode $ untile t0)
--     pure ()

-- t0 = VectorTile (DMZ.fromList [(pack "", l0)])
-- l0 = Layer 2 "water" DV.empty DV.empty (DV.fromList [f0]) 4096
-- f0 = VT.Feature 0 props pv
-- props = DMZ.fromList [("uid", I64 123), ("foo", St "bar"), ("cat", St "flew")]
-- pv = DV.fromList [yyy]
-- yyy = VG.Polygon xxx DV.empty
-- xxx = DVU.fromList ([(0, 0), (0,1), (1,1), (1,0), (0,0)] :: [(Int,Int)])
