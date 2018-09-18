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
import qualified Data.LineString                       as LineString
import qualified Data.Validation                       as Validation
import qualified Data.Vector                           as Vector

import qualified Data.Geometry.Simplify.DouglasPeucker as SimplifyDouglasPeucker
import qualified Data.Geometry.Types.Config            as TypesConfig

simplifyUsing :: TypesConfig.SimplificationAlgorithm -> Vector.Vector Geospatial.PointXY -> Vector.Vector Geospatial.PointXY
simplifyUsing TypesConfig.NoAlgorithm    = id
simplifyUsing TypesConfig.DouglasPeucker = SimplifyDouglasPeucker.douglasPeucker 10.0
simplifyUsing TypesConfig.Visvalingam    = id

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
      Geospatial.Line l                       -> simplifyLine algo l feature acc
      Geospatial.MultiLine ls                 -> simplifyLines algo ls feature acc
      Geospatial.Polygon _                    -> Vector.cons feature acc
      Geospatial.MultiPolygon _               -> Vector.cons feature acc
      Geospatial.Collection gs                -> Foldable.foldMap (\x -> simplifyFeature algo x feature acc) gs

simplifyLine :: TypesConfig.SimplificationAlgorithm -> Geospatial.GeoLine -> Geospatial.GeoFeature a -> Vector.Vector (Geospatial.GeoFeature a) -> Vector.Vector (Geospatial.GeoFeature a)
simplifyLine algo (Geospatial.GeoLine points) (Geospatial.GeoFeature bbox _ props fId) acc =
  case LineString.fromVector (createSimplifiedGeoPoint algo points) of
    Validation.Success res -> Vector.cons (Geospatial.GeoFeature bbox (Geospatial.Line (Geospatial.GeoLine res)) props fId) acc
    Validation.Failure _   -> acc

simplifyLines :: TypesConfig.SimplificationAlgorithm -> Geospatial.GeoMultiLine -> Geospatial.GeoFeature a -> Vector.Vector (Geospatial.GeoFeature a) -> Vector.Vector (Geospatial.GeoFeature a)
simplifyLines algo (Geospatial.GeoMultiLine multiLines) (Geospatial.GeoFeature bbox _ props fId) acc =
  case validationToEither of
    Right res -> Vector.cons (Geospatial.GeoFeature bbox (Geospatial.MultiLine (Geospatial.GeoMultiLine res)) props fId) acc
    Left _ -> acc
  where
    validationToEither = traverse (Validation.toEither . LineString.fromVector) simplifyMultiLines
    simplifyMultiLines = fmap (createSimplifiedGeoPoint algo) multiLines

createSimplifiedGeoPoint :: TypesConfig.SimplificationAlgorithm -> LineString.LineString Geospatial.GeoPositionWithoutCRS -> Vector.Vector Geospatial.GeoPositionWithoutCRS
createSimplifiedGeoPoint algo points = Geospatial.GeoPointXY <$> simplifyUsing algo (fmap Geospatial.retrieveXY (LineString.toVector points))


