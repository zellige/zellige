{-# LANGUAGE FlexibleContexts #-}

-- TODO Work out how to create instance of Unboxed Vector
-- TODO Change to linear ring for polygons.
-- TODO Change to valid segment (non empty vector?) for lines.

module Data.Geometry.Clip (
  createBoundingBox
, clipPoint
, clipPoints
, clipLinesLb
, clipLineCs
, clipLinesCs
, clipLinesQc
, clipLinesNLN
, clipPolygon
, clipPolygons
, clipPolygonQc
, clipPolygonsQc
, clipFeature
, clipFeatures
) where

import qualified Data.Aeson                                        as Aeson
import qualified Data.Foldable                                     as Foldable
import qualified Data.Geospatial                                   as Geospatial
import qualified Data.Vector                                       as Vector

import           Data.Geometry.Clip.Internal.LineCohenSutherland
import           Data.Geometry.Clip.Internal.LineLiangBarsky
import           Data.Geometry.Clip.Internal.LineNichollLeeNicholl
import           Data.Geometry.Clip.Internal.LineQuickClip
import           Data.Geometry.Clip.Internal.Point
import           Data.Geometry.Clip.Internal.Polygon
import           Data.Geometry.Clip.Internal.PolygonQuickClip
import           Data.Geometry.Types.Geography

createBoundingBox :: Word -> Int -> BoundingBox
createBoundingBox buffer extent = BoundingBox (-fiBuffer) (-fiBuffer) (fiExtent + fiBuffer) (fiExtent + fiBuffer)
  where
    fiBuffer = fromIntegral buffer
    fiExtent = fromIntegral extent

clipFeatures :: BoundingBox -> Vector.Vector (Geospatial.GeoFeature Aeson.Value) -> Vector.Vector (Geospatial.GeoFeature Aeson.Value)
clipFeatures bbox = Vector.foldr (\x acc -> clipFeature bbox (Geospatial._geometry x) x acc) Vector.empty

clipFeature :: BoundingBox -> Geospatial.GeospatialGeometry -> Geospatial.GeoFeature Aeson.Value -> Vector.Vector (Geospatial.GeoFeature Aeson.Value) -> Vector.Vector (Geospatial.GeoFeature Aeson.Value)
clipFeature bbox geometry feature acc =
  case geometry of
    Geospatial.NoGeometry     -> acc
    Geospatial.Point g        -> clipPoint bbox g feature acc
    Geospatial.MultiPoint g   -> clipPoints bbox g feature acc
    Geospatial.Line g         -> clipLineCs bbox g feature acc
    Geospatial.MultiLine g    -> clipLinesCs bbox g feature acc
    Geospatial.Polygon _      -> acc
    Geospatial.MultiPolygon _ -> acc
    Geospatial.Collection gs  -> Foldable.foldMap (\x -> clipFeature bbox x feature acc) gs

