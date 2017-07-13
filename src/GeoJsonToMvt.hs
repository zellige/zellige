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
import           Types

mvtExtents = 2048
mvtX = 28999
mvtY = 19781
zoom = 15
gtc = GoogleTileCoords mvtX mvtY zoom
bb = boundingBox gtc

geoJsonFeaturesToMvtFeatures :: [GJ.Feature] -> (DV.Vector (VT.Feature VG.Point), DV.Vector (VT.Feature VG.LineString), DV.Vector (VT.Feature VG.Polygon))
geoJsonFeaturesToMvtFeatures = F.foldMap convertFeature

convertFeature :: GJ.Feature -> (DV.Vector (VT.Feature VG.Point), DV.Vector (VT.Feature VG.LineString), DV.Vector (VT.Feature VG.Polygon))
convertFeature (GJ.Feature _ geom props fid) = go geom
  where
      go (GJ.Point p)                  = mkPoint . convertPoint $ p
      go (GJ.MultiPoint mpg)           = mkPoint . convertMultiPoint $ mpg
      go (GJ.LineString ls)            = mkLineString . convertLineString $ ls
      go (GJ.MultiLineString mls)      = mkLineString . convertMultiLineString $ mls
      go (GJ.Polygon poly)             = mkPolygon . convertPolygon $ poly
      go (GJ.MultiPolygon mp)          = mkPolygon . convertMultiPolygon $ mp
      go (GJ.GeometryCollection geoms) = F.foldMap go geoms
      mkPoint p       = (mkFeature p, mempty, mempty)
      mkLineString l  = (mempty, mkFeature l, mempty)
      mkPolygon o     = (mempty, mempty, mkFeature o)
      mkFeature geoms = DV.singleton $ VT.Feature (convertId fid) (convertProps props) (DV.fromList geoms)

convertPoint :: GJ.PointGeometry -> [VG.Point]
convertPoint = sciLatLongToPoints . GJ.coordinates

convertMultiPoint :: GJ.MultiPointGeometry -> [VG.Point]
convertMultiPoint = pointToMvt . GJ.points

convertLineString :: GJ.LineStringGeometry -> [VG.LineString]
convertLineString = lineToMvt . pure

convertMultiLineString :: GJ.MultiLineStringGeometry -> [VG.LineString]
convertMultiLineString (GJ.MultiLineStringGeometry mls) = lineToMvt mls

convertPolygon :: GJ.PolygonGeometry -> [VG.Polygon]
convertPolygon = polygonToMvt . pure

convertMultiPolygon :: GJ.MultiPolygonGeometry -> [VG.Polygon]
convertMultiPolygon (GJ.MultiPolygonGeometry polys) = polygonToMvt polys

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
sToF = toRealFloat

convertElems :: (t, A.Value) -> Maybe (t, VT.Val)
convertElems (k, A.String v) = Just (k, VT.St v)
convertElems (k, A.Number v) = Just (k, VT.Do (sToF v))
convertElems (k, A.Bool v) = Just (k, VT.B v)
convertElems _ = Nothing

sciLatLongToPoints :: [Scientific] -> [VG.Point]
sciLatLongToPoints [] = []
sciLatLongToPoints [_] = []
sciLatLongToPoints x = fmap (\(lat, lon) -> latLonToXYInTile mvtExtents bb (LatLon (sToF lat) (sToF lon))) (zip <*> tail $ x)

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

