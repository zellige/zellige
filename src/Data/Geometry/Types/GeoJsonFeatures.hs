{-# LANGUAGE BangPatterns #-}

module Data.Geometry.Types.GeoJsonFeatures where

import qualified Data.Aeson.Types                    as AesonTypes
import qualified Data.ByteString.Lazy                as ByteStringLazy
import qualified Data.Foldable                       as Foldable
import qualified Data.Geospatial                     as Geospatial
import qualified Data.HashMap.Strict                 as HashMapStrict
import qualified Data.LinearRing                     as LinearRing
import qualified Data.List                           as List
import           Data.Monoid
import qualified Data.Scientific                     as Scientific
import qualified Data.SeqHelper                      as SeqHelper
import qualified Data.Sequence                       as Sequence
import qualified Data.Text                           as Text
import qualified Data.Text.Encoding                  as TextEncoding
import           Prelude                             hiding (Left, Right)

import qualified Data.Geometry.VectorTile.VectorTile as VectorTile

sToF :: Scientific.Scientific -> Double
sToF = Scientific.toRealFloat

mkPoint :: Word -> AesonTypes.Value -> Sequence.Seq VectorTile.Point -> Sequence.Seq (VectorTile.Feature (Sequence.Seq VectorTile.Point)) -> Sequence.Seq (VectorTile.Feature (Sequence.Seq VectorTile.Point))
mkPoint fId props p = (Sequence.<|) (VectorTile.Feature fId (convertProps props) p)

mkLineString :: Word -> AesonTypes.Value -> Sequence.Seq VectorTile.LineString -> Sequence.Seq (VectorTile.Feature (Sequence.Seq VectorTile.LineString)) -> Sequence.Seq (VectorTile.Feature (Sequence.Seq VectorTile.LineString))
mkLineString fId props l = (Sequence.<|) (mkFeature fId props l)

mkPolygon :: Word -> AesonTypes.Value -> Sequence.Seq VectorTile.Polygon -> Sequence.Seq (VectorTile.Feature (Sequence.Seq VectorTile.Polygon)) -> Sequence.Seq (VectorTile.Feature (Sequence.Seq VectorTile.Polygon))
mkPolygon x props o = (Sequence.<|) (mkFeature x props o)

mkFeature :: Word -> AesonTypes.Value -> Sequence.Seq g -> VectorTile.Feature (Sequence.Seq g)
mkFeature fId props = VectorTile.Feature fId (convertProps props)

convertProps :: AesonTypes.Value -> HashMapStrict.HashMap ByteStringLazy.ByteString VectorTile.Val
convertProps (AesonTypes.Object !x) = HashMapStrict.foldrWithKey (\k v acc -> maybe acc (\(!k', !v') -> HashMapStrict.insert k' v' acc) (convertElems (k, v))) HashMapStrict.empty x
convertProps _                 = HashMapStrict.empty

-- data Val = St BL.ByteString | Fl Float | Do Double | I64 Int64 | W64 Word.Word64 | S64 Int64 | B Bool

convertElems :: (Text.Text, AesonTypes.Value) -> Maybe (ByteStringLazy.ByteString, VectorTile.Val)
convertElems (!k, AesonTypes.String !v) = Just ((ByteStringLazy.fromStrict . TextEncoding.encodeUtf8) k, VectorTile.St ((ByteStringLazy.fromStrict . TextEncoding.encodeUtf8) v))
convertElems (!k, AesonTypes.Number !v) = Just ((ByteStringLazy.fromStrict . TextEncoding.encodeUtf8) k, VectorTile.Do (sToF v))
convertElems (!k, AesonTypes.Bool !v)   = Just ((ByteStringLazy.fromStrict . TextEncoding.encodeUtf8) k, VectorTile.B v)
convertElems _                     = Nothing

-- Points

convertPoint :: Geospatial.GeoPoint -> Sequence.Seq VectorTile.Point
convertPoint = coordsToPoints . Geospatial._unGeoPoint

convertMultiPoint :: Geospatial.GeoMultiPoint -> Sequence.Seq VectorTile.Point
convertMultiPoint = Foldable.foldMap convertPoint . Geospatial.splitGeoMultiPoint

-- Lines

convertLineString :: Geospatial.GeoLine -> Sequence.Seq VectorTile.LineString
convertLineString l =
  if Sequence.length convertedLine > 1
    then Sequence.singleton $ VectorTile.LineString convertedLine
    else Sequence.empty
  where
    convertedLine = convertAndRemoveDupes . Geospatial._unGeoLine $ l

convertMultiLineString :: Geospatial.GeoMultiLine -> Sequence.Seq VectorTile.LineString
convertMultiLineString = Foldable.foldMap convertLineString . Geospatial.splitGeoMultiLine

-- Polygons

convertPolygon :: Geospatial.GeoPolygon -> Sequence.Seq VectorTile.Polygon
convertPolygon poly =
  Sequence.singleton $
  case Sequence.viewl rawPoly of
    Sequence.EmptyL -> VectorTile.Polygon mempty mempty
    (h Sequence.:< rest) ->
      if Sequence.length rest == 0 then
        mkPoly h
      else
        VectorTile.Polygon (convertAndRemoveDupes h) (mkPolys rest)
  where
    rawPoly = Geospatial._unGeoPolygon poly

-- Foldr?
mkPolys :: Foldable t => t (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS) -> Sequence.Seq VectorTile.Polygon
mkPolys = List.foldl' (\acc lring -> mkPoly lring Sequence.<| acc) Sequence.empty

mkPoly :: LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS -> VectorTile.Polygon
mkPoly lring = VectorTile.Polygon (convertAndRemoveDupes lring) mempty

convertMultiPolygon :: Geospatial.GeoMultiPolygon -> Sequence.Seq VectorTile.Polygon
convertMultiPolygon = Foldable.foldMap convertPolygon . Geospatial.splitGeoMultiPolygon

-- Helpers
-- Foldr?
convertAndRemoveDupes :: Foldable t => t Geospatial.GeoPositionWithoutCRS -> Sequence.Seq VectorTile.Point
convertAndRemoveDupes = SeqHelper.removeNextDuplicate . Foldable.foldMap coordsToPoints

coordsToPoints :: Geospatial.GeoPositionWithoutCRS -> Sequence.Seq VectorTile.Point
coordsToPoints geoPosition = Sequence.singleton newPoint
  where
    newPoint = VectorTile.Point (round posX) (round posY)
    (Geospatial.PointXY posX posY) = Geospatial.retrieveXY geoPosition
