{-# LANGUAGE OverloadedStrings #-}

module GeoJsonToMvt where

import           Data.Aeson
import qualified Data.Geography.GeoJSON          as GJ
import qualified Data.HashMap.Strict             as HM
import qualified Data.Map.Lazy                   as DMZ
import           Data.Maybe
import           Data.Monoid                     ((<>))
import           Data.Scientific
import qualified Data.Text                       as T (Text)
import qualified Data.Vector                     as DV
import qualified Data.Vector.Unboxed             as DVU
import qualified Geography.VectorTile.Geometry   as VG
import qualified Geography.VectorTile.VectorTile as VT

geoJsonFeaturesToMvtFeatures :: [GJ.Feature] -> ([VT.Feature VG.Point], [VT.Feature VG.LineString], [VT.Feature VG.Polygon])
geoJsonFeaturesToMvtFeatures = Prelude.foldr convertFeature ([], [], [])

convertFeature :: GJ.Feature -> ([VT.Feature VG.Point], [VT.Feature VG.LineString], [VT.Feature VG.Polygon]) -> ([VT.Feature VG.Point], [VT.Feature VG.LineString], [VT.Feature VG.Polygon])
convertFeature (GJ.Feature _ geom@(GJ.Point _) props fid) (p, l, o) = (mkFeature mvtPoint geom props fid : p, l, o)
convertFeature (GJ.Feature _ geom@(GJ.MultiPoint _) props fid) (p, l, o)  = (mkFeature mvtPoint geom props fid : p, l, o)
convertFeature (GJ.Feature _ geom@(GJ.LineString _) props fid) (p, l, o) = (p, mkFeature mvtLineString geom props fid : l, o)
convertFeature (GJ.Feature _ geom@(GJ.MultiLineString _) props fid) (p, l, o) = (p, mkFeature mvtLineString geom props fid : l, o)
convertFeature (GJ.Feature _ geom@(GJ.Polygon _) props fid) (p, l, o) = (p, l, mkFeature mvtPolygon geom props fid : o)
convertFeature (GJ.Feature _ geom@(GJ.MultiPolygon _) props fid) (p, l, o) = (p, l, mkFeature mvtPolygon geom props fid : o)
convertFeature (GJ.Feature bb (GJ.GeometryCollection geoms) props fid) plo = Prelude.foldr (\geom plo' -> convertFeature (GJ.Feature bb geom props fid) plo') plo geoms

data MVT a = MVTPoint { mvtPoint :: VG.Point }
  | MVTLineString { mvtLineString :: VG.LineString }
  | MVTPolygon { mvtPolygon :: VG.Polygon}

convertId :: Maybe Value -> Int
convertId (Just (Number n)) = (fToInt . sToF) n
convertId _ = 0

convertGeom :: GJ.Geometry -> DV.Vector (MVT a)
convertGeom (GJ.Point p) = MVTPoint <$> DV.fromList (moreTerrible $ GJ.coordinates p)
convertGeom (GJ.MultiPoint mpg) = MVTPoint <$> DV.fromList (blerg [] (GJ.points mpg))
convertGeom (GJ.LineString ls) = MVTLineString <$> DV.fromList (blergLine [] [ls])
convertGeom (GJ.MultiLineString (GJ.MultiLineStringGeometry mls)) = MVTLineString <$> DV.fromList (blergLine [] mls)
convertGeom (GJ.Polygon poly) = MVTPolygon <$> DV.fromList (blergPoly [] [poly])
convertGeom (GJ.MultiPolygon (GJ.MultiPolygonGeometry polys)) = MVTPolygon <$> DV.fromList (blergPoly [] polys)

mkFeature :: (MVT a -> g) -> GJ.Geometry -> Value -> Maybe Value -> VT.Feature g
mkFeature f geom props fid = VT.Feature (convertId fid) (convertProps props) (f <$> convertGeom geom)

blerg :: [VG.Point] -> [GJ.PointGeometry] -> [VG.Point]
blerg = Prelude.foldr (\pg acc -> moreTerrible (GJ.coordinates pg) <> acc)

blergLine :: [VG.LineString] -> [GJ.LineStringGeometry] -> [VG.LineString]
blergLine = Prelude.foldr (\lsg acc -> VG.LineString (DVU.fromList (blerg [] (GJ.lineString lsg))) : acc)

blergPoly :: [VG.Polygon] -> [GJ.PolygonGeometry] -> [VG.Polygon]
blergPoly = Prelude.foldr (\poly acc -> VG.Polygon (DVU.fromList (blerg [] (GJ.exterior poly))) (DV.fromList (blergPoly [] (Prelude.fmap (\x -> GJ.PolygonGeometry x []) (GJ.holes poly)))) : acc)

sToF :: Scientific -> Float
sToF n = toRealFloat n :: Float

fToInt :: Float -> Int
fToInt = round

terrible :: Scientific -> Int
terrible = fToInt . sToF

convertProps :: Value -> DMZ.Map T.Text VT.Val
convertProps (Object x) = DMZ.fromList $ catMaybes $ Prelude.fmap convertElems (HM.toList x)
convertProps _ = DMZ.empty

convertElems :: (t, Value) -> Maybe (t, VT.Val)
convertElems (k, String v) = Just (k, VT.St v)
convertElems (k, Number v) = Just (k, VT.Fl (sToF v))
convertElems (k, Bool v) = Just (k, VT.B v)
convertElems _ = Nothing

moreTerrible :: [Scientific] -> [VG.Point]
moreTerrible [] = []
moreTerrible (k:v:t) = (terrible k, terrible v) : moreTerrible t

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
