{-# LANGUAGE FlexibleContexts #-}

-- Liang Barsky Line Clipping Algorithm
-- https://en.wikipedia.org/wiki/Liang%E2%80%93Barsky_algorithm

module Data.Geometry.Clip.Internal.LiangBarskyLine (
 clipLines
 , foldLine
) where

import qualified Data.Vector                   as Vector
import qualified Geography.VectorTile          as VectorTile

import qualified Data.Geometry.Types.Geography as TypesGeography

data Edge = LeftEdge | RightEdge | BottomEdge | TopEdge
  deriving (Show, Eq, Enum)

clipLines :: TypesGeography.BoundingBoxPts -> Vector.Vector VectorTile.LineString -> Vector.Vector VectorTile.LineString
clipLines _ _ = undefined

foldLine :: TypesGeography.BoundingBoxPts -> TypesGeography.StorableLine -> Maybe VectorTile.Point
foldLine bb line = foldr (\edge acc -> calcT1AndT2OrQuit (calcPAndQ bb line edge) acc) (Just $ VectorTile.Point 0 1) [LeftEdge, RightEdge, BottomEdge, TopEdge]

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
calcPAndQ TypesGeography.BoundingBoxPts{TypesGeography._bbMinPts = (VectorTile.Point minX minY), TypesGeography._bbMaxPts = (VectorTile.Point maxX maxY)} (TypesGeography.StorableLine (VectorTile.Point x1 y1) (VectorTile.Point x2 y2)) e =
  case e of
    LeftEdge   -> VectorTile.Point (-1 * deltaX) (-1 * (minX - x1))
    RightEdge  -> VectorTile.Point deltaX (maxX - x1)
    BottomEdge -> VectorTile.Point (-1 * deltaY) (-1 * (minY - y1))
    TopEdge    -> VectorTile.Point deltaY (maxY - y1)
  where
    deltaX = x2 - x1
    deltaY = y2 - y1

