module GeoJsonToMvt where

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

import           SphericalMercator

mvtExtents = 2048
mvtX = 28999
mvtY = 19781
zoom = 15
bb = boundingBox mvtX mvtY zoom

geoJsonFeaturesToMvtFeatures :: [GJ.Feature] -> (DV.Vector (VT.Feature VG.Point), DV.Vector (VT.Feature VG.LineString), DV.Vector (VT.Feature VG.Polygon))
geoJsonFeaturesToMvtFeatures = F.foldMap convertFeature

convertFeature :: GJ.Feature -> (DV.Vector (VT.Feature VG.Point), DV.Vector (VT.Feature VG.LineString), DV.Vector (VT.Feature VG.Polygon))
convertFeature (GJ.Feature _ geom@(GJ.Point _) props fid) = (DV.singleton (mkFeature convertPoint geom props fid), DV.empty, DV.empty)
convertFeature (GJ.Feature _ geom@(GJ.MultiPoint _) props fid) = (DV.singleton (mkFeature convertPoint geom props fid), DV.empty, DV.empty)
convertFeature (GJ.Feature _ geom@(GJ.LineString _) props fid) = (DV.empty, DV.singleton (mkFeature convertLineString geom props fid), DV.empty)
convertFeature (GJ.Feature _ geom@(GJ.MultiLineString _) props fid) = (DV.empty, DV.singleton (mkFeature convertLineString geom props fid), DV.empty)
convertFeature (GJ.Feature _ geom@(GJ.Polygon _) props fid) = (DV.empty, DV.empty, DV.singleton (mkFeature convertPolygon geom props fid))
convertFeature (GJ.Feature _ geom@(GJ.MultiPolygon _) props fid) = (DV.empty, DV.empty, DV.singleton (mkFeature convertPolygon geom props fid))
convertFeature (GJ.Feature bb (GJ.GeometryCollection geoms) props fid) = F.foldMap (\geom -> convertFeature (GJ.Feature bb geom props fid)) geoms

convertPoint :: GJ.Geometry -> DV.Vector VG.Point
convertPoint (GJ.Point p) = DV.fromList (sciLatLongToPoints $ GJ.coordinates p)
convertPoint (GJ.MultiPoint mpg) = DV.fromList . pointToMvt $ GJ.points mpg

convertLineString :: GJ.Geometry -> DV.Vector VG.LineString
convertLineString (GJ.LineString ls) = DV.fromList (lineToMvt [ls])
convertLineString (GJ.MultiLineString (GJ.MultiLineStringGeometry mls)) = DV.fromList $ lineToMvt mls

convertPolygon :: GJ.Geometry -> DV.Vector VG.Polygon
convertPolygon (GJ.Polygon poly) = DV.fromList $ polygonToMvt [poly]
convertPolygon (GJ.MultiPolygon (GJ.MultiPolygonGeometry polys)) = DV.fromList $ polygonToMvt polys

mkFeature :: (GJ.Geometry -> DV.Vector g) -> GJ.Geometry -> A.Value -> Maybe A.Value -> VT.Feature g
mkFeature f geom props fid = VT.Feature (convertId fid) (convertProps props) (f geom)
  where
    convertProps :: A.Value -> DMZ.Map T.Text VT.Val
    convertProps (A.Object x) = DMZ.fromList . catMaybes $ Prelude.fmap convertElems (HM.toList x)
    convertProps _ = DMZ.empty

    convertId :: Maybe A.Value -> Int
    convertId (Just (A.Number n)) = (round . sToF) n
    convertId _ = 0

pointToMvt :: [GJ.PointGeometry] -> [VG.Point]
pointToMvt = F.foldMap (sciLatLongToPoints . GJ.coordinates)

lineToMvt :: [GJ.LineStringGeometry] -> [VG.LineString]
lineToMvt = F.foldMap (\lsg -> [VG.LineString . DVU.fromList . pointToMvt $ GJ.lineString lsg])

polygonToMvt :: [GJ.PolygonGeometry] -> [VG.Polygon]
polygonToMvt = F.foldMap (\poly -> [VG.Polygon (ext (GJ.exterior poly)) (int (GJ.holes poly))])
  where
    ext = DVU.fromList . pointToMvt
    int y = DV.fromList . polygonToMvt $ fmap (\x -> GJ.PolygonGeometry x []) y

sToF :: Scientific -> Double
sToF n = toRealFloat n

convertElems :: (t, A.Value) -> Maybe (t, VT.Val)
convertElems (k, A.String v) = Just (k, VT.St v)
convertElems (k, A.Number v) = Just (k, VT.Do (sToF v))
convertElems (k, A.Bool v) = Just (k, VT.B v)
convertElems _ = Nothing

sciLatLongToPoints :: [Scientific] -> [VG.Point]
sciLatLongToPoints [] = []
sciLatLongToPoints [_] = []
sciLatLongToPoints x = fmap (\(lat, lon) -> latLonToXYInTile mvtExtents bb (sToF lat, sToF lon)) (zip <*> tail $ x)

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

