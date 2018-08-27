{-# LANGUAGE FlexibleContexts #-}

-- Liang Barsky Line Clipping Algorithm
-- https://en.wikipedia.org/wiki/Liang%E2%80%93Barsky_algorithm

module Data.Geometry.Clip.Internal.LiangBarskyLine (
 clipLines
 , foldLine
) where

import qualified Data.Vector                      as Vector
import qualified Data.Vector.Storable             as VectorStorable
import qualified Geography.VectorTile             as VectorTile

import qualified Data.Geometry.Clip.Internal.Line as ClipLine
import qualified Data.Geometry.Types.Geography    as TypesGeography

data Edge = LeftEdge | RightEdge | BottomEdge | TopEdge
  deriving (Show, Eq, Enum)

clipLines :: TypesGeography.BoundingBoxPts -> Vector.Vector VectorTile.LineString -> Vector.Vector VectorTile.LineString
clipLines bb = foldr (\lineString acc -> maybeAddLine acc (lineToClippedPoints bb lineString)) Vector.empty

maybeAddLine :: Vector.Vector VectorTile.LineString -> VectorStorable.Vector VectorTile.Point -> Vector.Vector VectorTile.LineString
maybeAddLine acc pts =
    case ClipLine.checkValidLineString pts of
      Nothing  -> acc
      Just res -> Vector.cons res acc

lineToClippedPoints :: TypesGeography.BoundingBoxPts -> VectorTile.LineString -> VectorStorable.Vector VectorTile.Point
lineToClippedPoints bb lineString = foldPointsToLine $ VectorStorable.foldr (clipOrDiscard bb) VectorStorable.empty (ClipLine.getLines lineString)

foldPointsToLine :: VectorStorable.Vector TypesGeography.StorableLine -> VectorStorable.Vector VectorTile.Point
foldPointsToLine = VectorStorable.foldr (mappend . (\(TypesGeography.StorableLine p1 p2) -> VectorStorable.fromList [p1, p2])) mempty

clipOrDiscard :: TypesGeography.BoundingBoxPts -> TypesGeography.StorableLine -> VectorStorable.Vector TypesGeography.StorableLine -> VectorStorable.Vector TypesGeography.StorableLine
clipOrDiscard bb line acc =
  case foldLine bb line of
    Nothing -> acc
    Just pt -> VectorStorable.cons (recreateLine pt line) acc

foldLine :: TypesGeography.BoundingBoxPts -> TypesGeography.StorableLine -> Maybe VectorTile.Point
foldLine bb line = foldr (\edge acc -> calcT1AndT2OrQuit (calcPAndQ bb line edge) acc) (Just $ VectorTile.Point 0 1) [LeftEdge, RightEdge, BottomEdge, TopEdge]

recreateLine :: VectorTile.Point -> TypesGeography.StorableLine -> TypesGeography.StorableLine
recreateLine (VectorTile.Point t1 t2) line@(TypesGeography.StorableLine (VectorTile.Point x1 y1) _) =
  TypesGeography.StorableLine
    (VectorTile.Point (x1 + t1 * deltaX line) (y1 + t1 * deltaY line))
    (VectorTile.Point (x1 + t2 * deltaX line) (y1 + t2 * deltaY line))

calcT1AndT2OrQuit :: VectorTile.Point -> Maybe VectorTile.Point -> Maybe VectorTile.Point
calcT1AndT2OrQuit (VectorTile.Point p q) orig =
    case orig of
      Nothing -> Nothing
      Just (VectorTile.Point t1 t2) ->
        case compare p 0 of
          EQ | q < 0     -> Nothing
             | otherwise -> orig
          LT | r > t2    -> Nothing
             | r > t1    -> Just (VectorTile.Point r t2)
             | otherwise -> orig
          GT | r < t1    -> Nothing
             | r < t2    -> Just (VectorTile.Point t1 r)
             | otherwise -> orig
  where
    r = round ((fromIntegral q :: Double) / (fromIntegral p :: Double))

calcPAndQ :: TypesGeography.BoundingBoxPts -> TypesGeography.StorableLine -> Edge -> VectorTile.Point
calcPAndQ TypesGeography.BoundingBoxPts{TypesGeography._bbMinPts = (VectorTile.Point minX minY), TypesGeography._bbMaxPts = (VectorTile.Point maxX maxY)} line@(TypesGeography.StorableLine (VectorTile.Point x1 y1) _) e =
  case e of
    LeftEdge   -> VectorTile.Point (-1 * deltaX line) (x1 - minX)
    RightEdge  -> VectorTile.Point (deltaX line) (maxX - x1)
    BottomEdge -> VectorTile.Point (-1 * deltaY line) (y1 - minY)
    TopEdge    -> VectorTile.Point (deltaY line) (maxY - y1)

deltaX :: TypesGeography.StorableLine -> Int
deltaX (TypesGeography.StorableLine (VectorTile.Point x1 _) (VectorTile.Point x2 _)) = x2 - x1

deltaY :: TypesGeography.StorableLine -> Int
deltaY (TypesGeography.StorableLine (VectorTile.Point _ y1) (VectorTile.Point _ y2)) = y2 - y1
