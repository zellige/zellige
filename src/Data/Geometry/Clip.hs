{-# LANGUAGE FlexibleContexts #-}

-- TODO Work out how to create instance of Unboxed Vector
-- TODO Change to linear ring for polygons.
-- TODO Change to valid segment (non empty vector?) for lines.

module Data.Geometry.Clip (
  createBoundingBox
, clipPoint
, clipPoints
, clipLineLb
, clipLinesLb
, clipLineCs
, clipLinesCs
, clipLineQc
, clipLinesQc
, clipLineNLN
, clipLinesNLN
, clipPolygon
, clipPolygons
, clipPolygonQc
, clipPolygonsQc
, clipFeature
, clipFeatures
, clipPolygonNLNN
, clipPolygonsNLNN
, mapFeature
) where

import qualified Data.Aeson                                                as Aeson
import qualified Data.Foldable                                             as Foldable
import qualified Data.Geospatial                                           as Geospatial
import qualified Data.Sequence                                             as Sequence

import           Data.Geometry.Clip.Internal.LineCohenSutherland
import           Data.Geometry.Clip.Internal.LineLiangBarsky
import           Data.Geometry.Clip.Internal.LineNichollLeeNicholl
import           Data.Geometry.Clip.Internal.LineQuickClip
import           Data.Geometry.Clip.Internal.Point
import           Data.Geometry.Clip.Internal.Polygon
import           Data.Geometry.Clip.Internal.PolygonNichollLeeNichollNicol
import           Data.Geometry.Clip.Internal.PolygonQuickClip
import           Data.Geometry.Types.Geography

createBoundingBox :: Word -> Int -> BoundingBox
createBoundingBox buffer extent = BoundingBox (-fiBuffer) (-fiBuffer) (fiExtent + fiBuffer) (fiExtent + fiBuffer)
  where
    fiBuffer = fromIntegral buffer
    fiExtent = fromIntegral extent

clipFeatures :: BoundingBox -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value) -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value)
clipFeatures bbox = Foldable.foldr (\x acc -> clipFeature bbox (Geospatial._geometry x) x acc) Sequence.empty

clipFeature :: BoundingBox -> Geospatial.GeospatialGeometry -> Geospatial.GeoFeature Aeson.Value -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value) -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value)
clipFeature bbox geometry feature acc =
  case geometry of
    Geospatial.NoGeometry     -> acc
    Geospatial.Point g        -> clipPoint bbox g feature acc
    Geospatial.MultiPoint g   -> clipPoints bbox g feature acc
    Geospatial.Line g         -> clipLineQc bbox g feature acc
    Geospatial.MultiLine g    -> clipLinesQc bbox g feature acc
    Geospatial.Polygon g      -> clipPolygon bbox g feature acc
    Geospatial.MultiPolygon g -> clipPolygons bbox g feature acc
    Geospatial.Collection gs  -> Foldable.foldMap (\x -> clipFeature bbox x feature acc) gs

mapFeature :: BoundingBox -> Geospatial.GeospatialGeometry -> Geospatial.GeospatialGeometry
mapFeature bbox geometry =
  case geometry of
    Geospatial.NoGeometry     -> geometry
    Geospatial.Point g        -> maybe Geospatial.NoGeometry Geospatial.Point (clipPointMap bbox g)
    Geospatial.MultiPoint g   -> maybe Geospatial.NoGeometry Geospatial.MultiPoint (clipPointsMap bbox g)
    Geospatial.Line g         -> maybe Geospatial.NoGeometry Geospatial.Line (clipLineQcMap bbox g)
    Geospatial.MultiLine g    -> maybe Geospatial.NoGeometry Geospatial.MultiLine (clipLinesQcMap bbox g)
    Geospatial.Polygon g      -> maybe Geospatial.NoGeometry Geospatial.Polygon (clipPolygonMap bbox g)
    Geospatial.MultiPolygon g -> maybe Geospatial.NoGeometry Geospatial.MultiPolygon (clipPolygonsMap bbox g)
    Geospatial.Collection gs  -> if Sequence.null (foldOver gs) then Geospatial.NoGeometry else Geospatial.Collection (foldOver gs)
  where
    foldOver = Foldable.foldr (\geom acc -> mapFeature bbox geom Sequence.<| acc) Sequence.empty

