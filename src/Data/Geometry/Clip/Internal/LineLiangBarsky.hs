{-# LANGUAGE FlexibleContexts #-}

-- Liang Barsky Line Clipping Algorithm
-- https://en.wikipedia.org/wiki/Liang%E2%80%93Barsky_algorithm

module Data.Geometry.Clip.Internal.LineLiangBarsky (
  clipLineLb
  , clipLinesLb
) where

import qualified Data.Aeson                       as Aeson
import qualified Data.Foldable                    as Foldable
import qualified Data.Geospatial                  as Geospatial
import qualified Data.LineString                  as LineString
import qualified Data.Sequence                    as Sequence
import qualified Data.Validation                  as Validation
import           Prelude                          hiding (lines)

import qualified Data.Geometry.Clip.Internal.Line as ClipLine
import qualified Data.Geometry.Types.Geography    as TypesGeography

data Edge = LeftEdge | RightEdge | BottomEdge | TopEdge
  deriving (Show, Eq, Enum)

clipLineLb :: TypesGeography.BoundingBox -> Geospatial.GeoLine -> Geospatial.GeoFeature Aeson.Value -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value) -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value)
clipLineLb bb line feature acc =
  case LineString.fromSeq clippedLine of
    Validation.Success res -> (Sequence.<|) (Geospatial.reWrapGeometry feature (Geospatial.Line (Geospatial.GeoLine res))) acc
    Validation.Failure _   -> acc
  where
    clippedLine = ClipLine.lineToGeoPoint $ lineToClippedPoints bb line

clipLinesLb :: TypesGeography.BoundingBox -> Geospatial.GeoMultiLine -> Geospatial.GeoFeature Aeson.Value -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value) -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value)
clipLinesLb bb lines (Geospatial.GeoFeature bbox _ props fId) acc = checkLinesAndAdd
  where
    checkLinesAndAdd = if Sequence.null multiLine then acc else (Sequence.<|) reMakeFeature acc
    reMakeFeature = Geospatial.GeoFeature bbox (Geospatial.MultiLine (Geospatial.GeoMultiLine multiLine)) props fId
    multiLine = Foldable.foldl' maybeAddLine mempty (linesToClippedPoints bb (Geospatial.splitGeoMultiLine lines))

maybeAddLine :: Sequence.Seq
               (LineString.LineString Geospatial.GeoPositionWithoutCRS)
             -> Sequence.Seq TypesGeography.GeoStorableLine
             -> Sequence.Seq
                  (LineString.LineString Geospatial.GeoPositionWithoutCRS)
maybeAddLine acc pp =
  case clipLineToValidationLineString pp of
    Validation.Success res -> (Sequence.<|) res acc
    Validation.Failure _   -> acc

clipLineToValidationLineString :: Sequence.Seq TypesGeography.GeoStorableLine
             -> Validation.Validation
                LineString.SequenceToLineStringError (LineString.LineString Geospatial.GeoPositionWithoutCRS)
clipLineToValidationLineString lines = LineString.fromSeq (ClipLine.lineToGeoPoint lines)

lineToClippedPoints :: TypesGeography.BoundingBox -> Geospatial.GeoLine -> Sequence.Seq TypesGeography.GeoStorableLine
lineToClippedPoints bb geoLine = Foldable.foldr (clipOrDiscard bb) Sequence.empty (ClipLine.getLines geoLine)

linesToClippedPoints :: Functor f => TypesGeography.BoundingBox -> f Geospatial.GeoLine -> f (Sequence.Seq TypesGeography.GeoStorableLine)
linesToClippedPoints bb = fmap (lineToClippedPoints bb)

clipOrDiscard :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine -> Sequence.Seq TypesGeography.GeoStorableLine -> Sequence.Seq TypesGeography.GeoStorableLine
clipOrDiscard bb line acc =
  case foldLine bb line dXdY of
    Nothing -> acc
    Just pt -> (Sequence.<|) (recreateLine pt line dXdY) acc
  where
    dXdY = dXAndDyFromLine line

foldLine :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine -> DxAndDy -> Maybe T1AndT2
foldLine bb line dXdY = Foldable.foldl' (\acc edge -> calcT1AndT2OrQuit (calcPAndQ bb line dXdY edge) acc) t1AndT2 (Sequence.fromList [LeftEdge, RightEdge, BottomEdge, TopEdge])

data DxAndDy = DxAndDy !Double !Double
data T1AndT2 = T1AndT2 !Double !Double
data PAndQ = PAndQ !Double !Double

dXAndDyFromLine :: TypesGeography.GeoStorableLine -> DxAndDy
dXAndDyFromLine (TypesGeography.GeoStorableLine (Geospatial.PointXY x1 y1) (Geospatial.PointXY x2 y2)) = DxAndDy (x2 - x1) (y2 - y1)

t1AndT2 :: Maybe T1AndT2
t1AndT2 = Just (T1AndT2 0.0 1.0)

recreateLine :: T1AndT2 -> TypesGeography.GeoStorableLine -> DxAndDy -> TypesGeography.GeoStorableLine
recreateLine (T1AndT2 t1 t2) (TypesGeography.GeoStorableLine (Geospatial.PointXY x1 y1) _) (DxAndDy dX dY) =
  TypesGeography.GeoStorableLine
    (Geospatial.PointXY (x1 + t1 * dX) (y1 + t1 * dY))
    (Geospatial.PointXY (x1 + t2 * dX) (y1 + t2 * dY))

calcT1AndT2OrQuit :: PAndQ -> Maybe T1AndT2 -> Maybe T1AndT2
calcT1AndT2OrQuit (PAndQ p q) orig =
    case orig of
      Nothing -> Nothing
      Just (T1AndT2 t1 t2) ->
        case compare p 0 of
          EQ | q < 0     -> Nothing
             | otherwise -> orig
          LT | r > t2    -> Nothing
             | r > t1    -> Just (T1AndT2 r t2)
             | otherwise -> orig
          GT | r < t1    -> Nothing
             | r < t2    -> Just (T1AndT2 t1 r)
             | otherwise -> orig
  where
    r = q / p

calcPAndQ :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine -> DxAndDy -> Edge -> PAndQ
calcPAndQ (TypesGeography.BoundingBox minX minY maxX maxY) (TypesGeography.GeoStorableLine (Geospatial.PointXY x1 y1) _) (DxAndDy dX dY) e =
  case e of
    LeftEdge   -> PAndQ (-1 * dX) (x1 - minX)
    RightEdge  -> PAndQ dX (maxX - x1)
    BottomEdge -> PAndQ (-1 * dY) (y1 - minY)
    TopEdge    -> PAndQ dY (maxY - y1)
