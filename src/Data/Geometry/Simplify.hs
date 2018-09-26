{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

module Data.Geometry.Simplify where

import qualified Data.Aeson                            as Aeson
import qualified Data.Foldable                         as Foldable
import qualified Data.Geometry.Simplify.DouglasPeucker as SimplifyDouglasPeucker
import qualified Data.Geometry.Types.Config            as TypesConfig
import qualified Data.Geospatial                       as Geospatial
import qualified Data.LinearRing                       as LinearRing
import qualified Data.LineString                       as LineString
import qualified Data.List.NonEmpty                    as ListNonEmpty
import qualified Data.Sequence                         as Sequence
import qualified Data.Validation                       as Validation

simplifyFeatures :: TypesConfig.SimplificationAlgorithm -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value) -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value)
simplifyFeatures algo = foldr (\x acc -> simplifyFeature algo (Geospatial._geometry x) x acc) Sequence.empty

simplifyFeature :: TypesConfig.SimplificationAlgorithm -> Geospatial.GeospatialGeometry -> Geospatial.GeoFeature Aeson.Value -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value) -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value)
simplifyFeature algo geometry feature acc =
  if algo == TypesConfig.NoAlgorithm then
    feature Sequence.<| acc
  else
    case geometry of
      Geospatial.NoGeometry                   -> acc
      Geospatial.Point _                      -> feature Sequence.<| acc
      Geospatial.MultiPoint _                 -> feature Sequence.<| acc
      Geospatial.Line l                       -> simplifyLineAcc algo l feature acc
      Geospatial.MultiLine ls                 -> simplifyLinesAcc algo ls feature acc
      Geospatial.Polygon p                    -> simplifyPolygonAcc algo p feature acc
      Geospatial.MultiPolygon ps              -> simplifyPolygonsAcc algo ps feature acc
      Geospatial.Collection gs                -> Foldable.foldMap (\x -> simplifyFeature algo x feature acc) gs

mapFeature :: TypesConfig.SimplificationAlgorithm -> Geospatial.GeospatialGeometry -> Geospatial.GeospatialGeometry
mapFeature algo geometry =
  if algo == TypesConfig.NoAlgorithm then
    geometry
  else
    case geometry of
      Geospatial.NoGeometry                                   -> geometry
      Geospatial.Point _                                      -> geometry
      Geospatial.MultiPoint _                                 -> geometry
      Geospatial.Line l                                       -> either (const Geospatial.NoGeometry) (Geospatial.Line . Geospatial.GeoLine) (simplifyLine algo l)
      Geospatial.MultiLine (Geospatial.GeoMultiLine ls)       -> either (const Geospatial.NoGeometry) (Geospatial.MultiLine  . Geospatial.GeoMultiLine) (simplifyLines algo ls)
      Geospatial.Polygon (Geospatial.GeoPolygon p)            -> either (const Geospatial.NoGeometry) (Geospatial.Polygon  . Geospatial.GeoPolygon) (simplifyPolygon algo p)
      Geospatial.MultiPolygon (Geospatial.GeoMultiPolygon ps) -> either (const Geospatial.NoGeometry) (Geospatial.MultiPolygon  . Geospatial.GeoMultiPolygon) (simplifyPolygons algo ps)
      Geospatial.Collection gs                                -> if Sequence.null (foldOver gs) then Geospatial.NoGeometry else Geospatial.Collection (foldOver gs)
   where
     foldOver = foldr (\geom acc -> mapFeature algo geom Sequence.<| acc) Sequence.empty

simplifyUsing :: TypesConfig.SimplificationAlgorithm -> Sequence.Seq Geospatial.PointXY -> Sequence.Seq Geospatial.PointXY
simplifyUsing TypesConfig.NoAlgorithm    = id
simplifyUsing TypesConfig.DouglasPeucker = SimplifyDouglasPeucker.douglasPeucker 1.0
simplifyUsing TypesConfig.Visvalingam    = id

simplifyLineAcc :: TypesConfig.SimplificationAlgorithm -> Geospatial.GeoLine -> Geospatial.GeoFeature a -> Sequence.Seq (Geospatial.GeoFeature a) -> Sequence.Seq (Geospatial.GeoFeature a)
simplifyLineAcc algo line (Geospatial.GeoFeature bbox _ props fId) acc =
  case simplifyLine algo line of
    Right res -> (Geospatial.GeoFeature bbox (Geospatial.Line (Geospatial.GeoLine res)) props fId) Sequence.<| acc
    Left _    -> acc

simplifyLine :: TypesConfig.SimplificationAlgorithm -> Geospatial.GeoLine -> Either LineString.VectorToLineStringError (LineString.LineString Geospatial.GeoPositionWithoutCRS)
simplifyLine algo (Geospatial.GeoLine points) = Validation.toEither $ LineString.fromSeq (createSimplifiedLineString algo points)

simplifyLinesAcc :: TypesConfig.SimplificationAlgorithm -> Geospatial.GeoMultiLine -> Geospatial.GeoFeature a -> Sequence.Seq (Geospatial.GeoFeature a) -> Sequence.Seq (Geospatial.GeoFeature a)
simplifyLinesAcc algo (Geospatial.GeoMultiLine multiLines) (Geospatial.GeoFeature bbox _ props fId) acc =
  case simplifyLines algo multiLines of
    Right res -> (Geospatial.GeoFeature bbox (Geospatial.MultiLine (Geospatial.GeoMultiLine res)) props fId) Sequence.<| acc
    Left _    -> acc

simplifyLines :: Traversable t => TypesConfig.SimplificationAlgorithm -> t (LineString.LineString Geospatial.GeoPositionWithoutCRS) -> Either LineString.VectorToLineStringError (t (LineString.LineString Geospatial.GeoPositionWithoutCRS))
simplifyLines algo multiLines = traverse (Validation.toEither . LineString.fromSeq) simplifyMultiLines
  where
    simplifyMultiLines = fmap (createSimplifiedLineString algo) multiLines

simplifyPolygonAcc :: TypesConfig.SimplificationAlgorithm -> Geospatial.GeoPolygon -> Geospatial.GeoFeature a -> Sequence.Seq (Geospatial.GeoFeature a) -> Sequence.Seq (Geospatial.GeoFeature a)
simplifyPolygonAcc algo (Geospatial.GeoPolygon polygon) (Geospatial.GeoFeature bbox _ props fId) acc =
  case simplifyPolygon algo polygon of
    Right res -> (Geospatial.GeoFeature bbox (Geospatial.Polygon (Geospatial.GeoPolygon res)) props fId) Sequence.<| acc
    Left _    -> acc

simplifyPolygon :: Traversable t => TypesConfig.SimplificationAlgorithm -> t (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS) -> Either (ListNonEmpty.NonEmpty (LinearRing.VectorToLinearRingError Geospatial.GeoPositionWithoutCRS)) (t (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS))
simplifyPolygon algo polygon = traverse (Validation.toEither . LinearRing.fromSeq) simplifyGeoPolygon
  where
    simplifyGeoPolygon = fmap (createSimplifiedLinearRing algo) polygon

simplifyPolygonsAcc :: TypesConfig.SimplificationAlgorithm -> Geospatial.GeoMultiPolygon -> Geospatial.GeoFeature a -> Sequence.Seq (Geospatial.GeoFeature a) -> Sequence.Seq (Geospatial.GeoFeature a)
simplifyPolygonsAcc algo (Geospatial.GeoMultiPolygon polygons) (Geospatial.GeoFeature bbox _ props fId) acc =
  case simplifyPolygons algo polygons of
    Right res -> (Geospatial.GeoFeature bbox (Geospatial.MultiPolygon (Geospatial.GeoMultiPolygon res)) props fId) Sequence.<| acc
    Left _    -> acc

simplifyPolygons :: (Traversable t1, Traversable t2) => TypesConfig.SimplificationAlgorithm -> t1 (t2 (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS)) -> Either (ListNonEmpty.NonEmpty (LinearRing.VectorToLinearRingError Geospatial.GeoPositionWithoutCRS))
         (t1 (t2 (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS)))
simplifyPolygons algo polygons = traverse (traverse (Validation.toEither . LinearRing.fromSeq)) simplifyGeoPolygons
  where
    simplifyGeoPolygons = (fmap . fmap) (createSimplifiedLinearRing algo) polygons

createSimplifiedLineString :: TypesConfig.SimplificationAlgorithm -> LineString.LineString Geospatial.GeoPositionWithoutCRS -> Sequence.Seq Geospatial.GeoPositionWithoutCRS
createSimplifiedLineString algo lineString = fmap Geospatial.GeoPointXY (simplifyUsing algo (fmap Geospatial.retrieveXY (LineString.toSeq lineString)))

createSimplifiedLinearRing :: TypesConfig.SimplificationAlgorithm -> LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS -> Sequence.Seq Geospatial.GeoPositionWithoutCRS
createSimplifiedLinearRing algo linearRing = fmap Geospatial.GeoPointXY (simplifyUsing algo (fmap Geospatial.retrieveXY (LinearRing.toSeq linearRing)))
