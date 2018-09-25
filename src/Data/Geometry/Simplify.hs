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
import qualified Data.Geospatial                       as Geospatial
import qualified Data.LinearRing                       as LinearRing
import qualified Data.LineString                       as LineString
import qualified Data.List.NonEmpty                    as ListNonEmpty
import qualified Data.Validation                       as Validation
import qualified Data.Vector                           as Vector
import qualified Data.Vector.Storable                  as VectorStorable

import qualified Data.Geometry.Simplify.DouglasPeucker as SimplifyDouglasPeucker
import qualified Data.Geometry.Types.Config            as TypesConfig

simplifyFeatures :: TypesConfig.SimplificationAlgorithm -> Vector.Vector (Geospatial.GeoFeature Aeson.Value) -> Vector.Vector (Geospatial.GeoFeature Aeson.Value)
simplifyFeatures algo = Vector.foldr (\x acc -> simplifyFeature algo (Geospatial._geometry x) x acc) Vector.empty

simplifyFeature :: TypesConfig.SimplificationAlgorithm -> Geospatial.GeospatialGeometry -> Geospatial.GeoFeature Aeson.Value -> Vector.Vector (Geospatial.GeoFeature Aeson.Value) -> Vector.Vector (Geospatial.GeoFeature Aeson.Value)
simplifyFeature algo geometry feature acc =
  if algo == TypesConfig.NoAlgorithm then
    Vector.cons feature acc
  else
    case geometry of
      Geospatial.NoGeometry                   -> acc
      Geospatial.Point _                      -> Vector.cons feature acc
      Geospatial.MultiPoint _                 -> Vector.cons feature acc
      Geospatial.Line l                       -> simplifyLineAcc algo l feature acc
      Geospatial.MultiLine ls                 -> simplifyLinesAcc algo ls feature acc
      Geospatial.Polygon p                    -> simplifyPolygonAcc algo p feature acc
      Geospatial.MultiPolygon ps              -> simplifyPolygonsAcc algo ps feature acc
      Geospatial.Collection gs                -> Foldable.foldMap (\x -> simplifyFeature algo x feature acc) gs

mapFeature :: TypesConfig.SimplificationAlgorithm -> Geospatial.GeospatialGeometry -> Maybe Geospatial.GeospatialGeometry
mapFeature algo geometry =
  if algo == TypesConfig.NoAlgorithm then
    Just geometry
  else
    case geometry of
      Geospatial.NoGeometry                                   -> Just geometry
      Geospatial.Point _                                      -> Just geometry
      Geospatial.MultiPoint _                                 -> Just geometry
      Geospatial.Line l                                       -> either (const Nothing) (Just . Geospatial.Line . Geospatial.GeoLine) (simplifyLine algo l)
      Geospatial.MultiLine (Geospatial.GeoMultiLine ls)       -> either (const Nothing) (Just . Geospatial.MultiLine  . Geospatial.GeoMultiLine) (simplifyLines algo ls)
      Geospatial.Polygon (Geospatial.GeoPolygon p)            -> either (const Nothing) (Just . Geospatial.Polygon  . Geospatial.GeoPolygon) (simplifyPolygon algo p)
      Geospatial.MultiPolygon (Geospatial.GeoMultiPolygon ps) -> either (const Nothing) (Just . Geospatial.MultiPolygon  . Geospatial.GeoMultiPolygon) (simplifyPolygons algo ps)
      Geospatial.Collection gs                                -> if Vector.null (foldOver gs) then Nothing else Just (Geospatial.Collection (foldOver gs))
   where
     foldOver = Vector.foldr (\geom acc -> maybe acc (`Vector.cons` acc) (mapFeature algo geom)) Vector.empty

simplifyUsing :: TypesConfig.SimplificationAlgorithm -> VectorStorable.Vector Geospatial.PointXY -> VectorStorable.Vector Geospatial.PointXY
simplifyUsing TypesConfig.NoAlgorithm    = id
simplifyUsing TypesConfig.DouglasPeucker = SimplifyDouglasPeucker.newDouglasPeucker 1.0
simplifyUsing TypesConfig.Visvalingam    = id

simplifyLineAcc :: TypesConfig.SimplificationAlgorithm -> Geospatial.GeoLine -> Geospatial.GeoFeature a -> Vector.Vector (Geospatial.GeoFeature a) -> Vector.Vector (Geospatial.GeoFeature a)
simplifyLineAcc algo line (Geospatial.GeoFeature bbox _ props fId) acc =
  case simplifyLine algo line of
    Right res -> Vector.cons (Geospatial.GeoFeature bbox (Geospatial.Line (Geospatial.GeoLine res)) props fId) acc
    Left _    -> acc

simplifyLine :: TypesConfig.SimplificationAlgorithm -> Geospatial.GeoLine -> Either LineString.VectorToLineStringError (LineString.LineString Geospatial.GeoPositionWithoutCRS)
simplifyLine algo (Geospatial.GeoLine points) = Validation.toEither $ LineString.fromVector (createSimplifiedLineString algo points)

simplifyLinesAcc :: TypesConfig.SimplificationAlgorithm -> Geospatial.GeoMultiLine -> Geospatial.GeoFeature a -> Vector.Vector (Geospatial.GeoFeature a) -> Vector.Vector (Geospatial.GeoFeature a)
simplifyLinesAcc algo (Geospatial.GeoMultiLine multiLines) (Geospatial.GeoFeature bbox _ props fId) acc =
  case simplifyLines algo multiLines of
    Right res -> Vector.cons (Geospatial.GeoFeature bbox (Geospatial.MultiLine (Geospatial.GeoMultiLine res)) props fId) acc
    Left _    -> acc

simplifyLines :: Traversable t => TypesConfig.SimplificationAlgorithm -> t (LineString.LineString Geospatial.GeoPositionWithoutCRS) -> Either LineString.VectorToLineStringError (t (LineString.LineString Geospatial.GeoPositionWithoutCRS))
simplifyLines algo multiLines = traverse (Validation.toEither . LineString.fromVector) simplifyMultiLines
  where
    simplifyMultiLines = fmap (createSimplifiedLineString algo) multiLines

simplifyPolygonAcc :: TypesConfig.SimplificationAlgorithm -> Geospatial.GeoPolygon -> Geospatial.GeoFeature a -> Vector.Vector (Geospatial.GeoFeature a) -> Vector.Vector (Geospatial.GeoFeature a)
simplifyPolygonAcc algo (Geospatial.GeoPolygon polygon) (Geospatial.GeoFeature bbox _ props fId) acc =
  case simplifyPolygon algo polygon of
    Right res -> Vector.cons (Geospatial.GeoFeature bbox (Geospatial.Polygon (Geospatial.GeoPolygon res)) props fId) acc
    Left _    -> acc

simplifyPolygon :: Traversable t => TypesConfig.SimplificationAlgorithm -> t (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS) -> Either (ListNonEmpty.NonEmpty (LinearRing.VectorToLinearRingError Geospatial.GeoPositionWithoutCRS)) (t (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS))
simplifyPolygon algo polygon = traverse (Validation.toEither . LinearRing.fromVector) simplifyGeoPolygon
  where
    simplifyGeoPolygon = fmap (createSimplifiedLinearRing algo) polygon

simplifyPolygonsAcc :: TypesConfig.SimplificationAlgorithm -> Geospatial.GeoMultiPolygon -> Geospatial.GeoFeature a -> Vector.Vector (Geospatial.GeoFeature a) -> Vector.Vector (Geospatial.GeoFeature a)
simplifyPolygonsAcc algo (Geospatial.GeoMultiPolygon polygons) (Geospatial.GeoFeature bbox _ props fId) acc =
  case simplifyPolygons algo polygons of
    Right res -> Vector.cons (Geospatial.GeoFeature bbox (Geospatial.MultiPolygon (Geospatial.GeoMultiPolygon res)) props fId) acc
    Left _    -> acc

simplifyPolygons :: (Traversable t1, Traversable t2) => TypesConfig.SimplificationAlgorithm -> t1 (t2 (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS)) -> Either (ListNonEmpty.NonEmpty (LinearRing.VectorToLinearRingError Geospatial.GeoPositionWithoutCRS))
         (t1 (t2 (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS)))
simplifyPolygons algo polygons = traverse (traverse (Validation.toEither . LinearRing.fromVector)) simplifyGeoPolygons
  where
    simplifyGeoPolygons = (fmap . fmap) (createSimplifiedLinearRing algo) polygons

createSimplifiedLineString :: TypesConfig.SimplificationAlgorithm -> LineString.LineString Geospatial.GeoPositionWithoutCRS -> VectorStorable.Vector Geospatial.GeoPositionWithoutCRS
createSimplifiedLineString algo lineString = VectorStorable.map Geospatial.GeoPointXY (simplifyUsing algo (VectorStorable.map Geospatial.retrieveXY (LineString.toVector lineString)))

createSimplifiedLinearRing :: TypesConfig.SimplificationAlgorithm -> LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS -> VectorStorable.Vector Geospatial.GeoPositionWithoutCRS
createSimplifiedLinearRing algo linearRing = VectorStorable.map Geospatial.GeoPointXY (simplifyUsing algo (VectorStorable.map Geospatial.retrieveXY (LinearRing.toVector linearRing)))
