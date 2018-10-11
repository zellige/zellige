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
      Geospatial.Line l                                       -> maybe Geospatial.NoGeometry (Geospatial.Line . Geospatial.GeoLine) (simplifyLine algo l)
      Geospatial.MultiLine (Geospatial.GeoMultiLine ls)       -> maybe Geospatial.NoGeometry (Geospatial.MultiLine  . Geospatial.GeoMultiLine) (simplifyLines algo ls)
      Geospatial.Polygon (Geospatial.GeoPolygon p)            -> maybe Geospatial.NoGeometry (Geospatial.Polygon  . Geospatial.GeoPolygon) (simplifyPolygon algo p)
      Geospatial.MultiPolygon (Geospatial.GeoMultiPolygon ps) -> maybe Geospatial.NoGeometry (Geospatial.MultiPolygon  . Geospatial.GeoMultiPolygon) (simplifyPolygons algo ps)
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
    Just res -> Geospatial.GeoFeature bbox (Geospatial.Line (Geospatial.GeoLine res)) props fId Sequence.<| acc
    Nothing  -> acc

simplifyLine :: TypesConfig.SimplificationAlgorithm -> Geospatial.GeoLine -> Maybe (LineString.LineString Geospatial.GeoPositionWithoutCRS)
simplifyLine algo (Geospatial.GeoLine points) = either (const Nothing) Just . Validation.toEither $ LineString.fromSeq (createSimplifiedLineString algo points)

simplifyLinesAcc :: TypesConfig.SimplificationAlgorithm -> Geospatial.GeoMultiLine -> Geospatial.GeoFeature a -> Sequence.Seq (Geospatial.GeoFeature a) -> Sequence.Seq (Geospatial.GeoFeature a)
simplifyLinesAcc algo (Geospatial.GeoMultiLine multiLines) (Geospatial.GeoFeature bbox _ props fId) acc =
  case simplifyLines algo multiLines of
    Just res -> Geospatial.GeoFeature bbox (Geospatial.MultiLine (Geospatial.GeoMultiLine res)) props fId Sequence.<| acc
    Nothing  -> acc

simplifyLines :: Traversable t => TypesConfig.SimplificationAlgorithm -> t (LineString.LineString Geospatial.GeoPositionWithoutCRS) -> Maybe (Sequence.Seq (LineString.LineString Geospatial.GeoPositionWithoutCRS))
simplifyLines algo multiLines =
  if Sequence.null foldLines
    then Nothing
    else Just foldLines
  where
    foldLines = Foldable.foldr (\points acc -> either (const acc) (Sequence.<| acc) (Validation.toEither . LineString.fromSeq $ createSimplifiedLineString algo points)) Sequence.empty multiLines

simplifyPolygonAcc :: TypesConfig.SimplificationAlgorithm -> Geospatial.GeoPolygon -> Geospatial.GeoFeature a -> Sequence.Seq (Geospatial.GeoFeature a) -> Sequence.Seq (Geospatial.GeoFeature a)
simplifyPolygonAcc algo (Geospatial.GeoPolygon polygon) (Geospatial.GeoFeature bbox _ props fId) acc =
  case simplifyPolygon algo polygon of
    Just res -> Geospatial.GeoFeature bbox (Geospatial.Polygon (Geospatial.GeoPolygon res)) props fId Sequence.<| acc
    Nothing  -> acc

simplifyPolygon :: Traversable t => TypesConfig.SimplificationAlgorithm -> t (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS) -> Maybe (Sequence.Seq (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS))
simplifyPolygon algo polygon =
  if Sequence.null simplifyGeoPolygon
    then Nothing
    else Just simplifyGeoPolygon
  where
    simplifyGeoPolygon = Foldable.foldr (\points acc -> either (const acc) (Sequence.<| acc) (Validation.toEither . LinearRing.fromSeq $ createSimplifiedLinearRing algo points)) Sequence.empty polygon

simplifyPolygonsAcc :: TypesConfig.SimplificationAlgorithm -> Geospatial.GeoMultiPolygon -> Geospatial.GeoFeature a -> Sequence.Seq (Geospatial.GeoFeature a) -> Sequence.Seq (Geospatial.GeoFeature a)
simplifyPolygonsAcc algo (Geospatial.GeoMultiPolygon polygons) (Geospatial.GeoFeature bbox _ props fId) acc =
  case simplifyPolygons algo polygons of
    Just res -> Geospatial.GeoFeature bbox (Geospatial.MultiPolygon (Geospatial.GeoMultiPolygon res)) props fId Sequence.<| acc
    Nothing  -> acc

simplifyPolygons :: (Traversable t1, Traversable t2) => TypesConfig.SimplificationAlgorithm -> t1 (t2 (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS)) -> Maybe (Sequence.Seq (Sequence.Seq (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS)))
simplifyPolygons algo polygons =
  if Sequence.null foldedPolys
    then Nothing
    else Just foldedPolys
  where
    foldedPolys = Foldable.foldr (\polys acc -> maybe acc (Sequence.<| acc) polys) Sequence.empty simplifyGeoPolygons
    simplifyGeoPolygons = fmap (simplifyPolygon algo) polygons

createSimplifiedLineString :: TypesConfig.SimplificationAlgorithm -> LineString.LineString Geospatial.GeoPositionWithoutCRS -> Sequence.Seq Geospatial.GeoPositionWithoutCRS
createSimplifiedLineString algo lineString = fmap Geospatial.GeoPointXY (simplifyUsing algo (fmap Geospatial.retrieveXY (LineString.toSeq lineString)))

createSimplifiedLinearRing :: TypesConfig.SimplificationAlgorithm -> LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS -> Sequence.Seq Geospatial.GeoPositionWithoutCRS
createSimplifiedLinearRing algo linearRing = fmap Geospatial.GeoPointXY (simplifyUsing algo (fmap Geospatial.retrieveXY (LinearRing.toSeq linearRing)))
