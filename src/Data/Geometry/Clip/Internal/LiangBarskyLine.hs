{-# LANGUAGE FlexibleContexts #-}

-- Liang Barsky Line Clipping Algorithm
-- https://en.wikipedia.org/wiki/Liang%E2%80%93Barsky_algorithm

module Data.Geometry.Clip.Internal.LiangBarskyLine (
  clipLinesLb
) where

import qualified Data.Vector                      as Vector
import qualified Data.Vector.Storable             as VectorStorable
import qualified Geography.VectorTile             as VectorTile

import qualified Data.Geometry.Clip.Internal.Line as ClipLine
import qualified Data.Geometry.Types.Geography    as TypesGeography

data Edge = LeftEdge | RightEdge | BottomEdge | TopEdge
  deriving (Show, Eq, Enum)

clipLinesLb :: TypesGeography.BoundingBoxPts -> Vector.Vector VectorTile.LineString -> Vector.Vector VectorTile.LineString
clipLinesLb bb = Vector.foldl' (\acc lineString -> maybeAddLine acc (lineToClippedPoints (TypesGeography.bboxPtsToBbox bb) lineString)) Vector.empty

maybeAddLine :: Vector.Vector VectorTile.LineString -> VectorStorable.Vector VectorTile.Point -> Vector.Vector VectorTile.LineString
maybeAddLine acc pts =
    case ClipLine.checkValidLineString pts of
      Nothing  -> acc
      Just res -> Vector.cons res acc

lineToClippedPoints :: TypesGeography.BoundingBox -> VectorTile.LineString -> VectorStorable.Vector VectorTile.Point
lineToClippedPoints bb lineString = foldPointsToLine $ VectorStorable.foldr (clipOrDiscard bb) VectorStorable.empty (ClipLine.getLines lineString)

foldPointsToLine :: VectorStorable.Vector TypesGeography.StorableLine -> VectorStorable.Vector VectorTile.Point
foldPointsToLine = VectorStorable.foldr (mappend . (\(TypesGeography.StorableLine p1 p2) -> VectorStorable.fromList [p1, p2])) mempty

clipOrDiscard :: TypesGeography.BoundingBox -> TypesGeography.StorableLine -> VectorStorable.Vector TypesGeography.StorableLine -> VectorStorable.Vector TypesGeography.StorableLine
clipOrDiscard bb line acc =
  case foldLine bb line of
    Nothing -> acc
    Just pt -> VectorStorable.cons (recreateLine pt line) acc

foldLine :: TypesGeography.BoundingBox -> TypesGeography.StorableLine -> Maybe T1AndT2
foldLine bb line = foldl (\acc edge -> calcT1AndT2OrQuit (calcPAndQ bb line edge) acc) newT1AndT2 [LeftEdge, RightEdge, BottomEdge, TopEdge]

data T1AndT2 = T1AndT2
  { _t1 :: !Double
  , _t2 :: !Double
  } deriving (Show, Eq)

newT1AndT2 :: Maybe T1AndT2
newT1AndT2 = Just (T1AndT2 0.0 1.0)

recreateLine :: T1AndT2 -> TypesGeography.StorableLine -> TypesGeography.StorableLine
recreateLine (T1AndT2 t1 t2) line@(TypesGeography.StorableLine (VectorTile.Point x1 y1) _) =
  TypesGeography.StorableLine
    (VectorTile.Point (round $ fromIntegral x1 + t1 * deltaX line) (round $ fromIntegral y1 + t1 * deltaY line))
    (VectorTile.Point (round $ fromIntegral x1 + t2 * deltaX line) (round $ fromIntegral y1 + t2 * deltaY line))

calcT1AndT2OrQuit :: (Double, Double) -> Maybe T1AndT2 -> Maybe T1AndT2
calcT1AndT2OrQuit (p, q) orig =
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

calcPAndQ :: TypesGeography.BoundingBox -> TypesGeography.StorableLine -> Edge -> (Double, Double)
calcPAndQ (TypesGeography.BoundingBox minX minY maxX maxY) line@(TypesGeography.StorableLine (VectorTile.Point x1 y1) _) e =
  case e of
    LeftEdge   -> (-1 * deltaX line, fromIntegral x1 - minX)
    RightEdge  -> (deltaX line, maxX - fromIntegral x1)
    BottomEdge -> (-1 * deltaY line, fromIntegral y1 - minY)
    TopEdge    -> (deltaY line, maxY - fromIntegral y1)

deltaX :: TypesGeography.StorableLine -> Double
deltaX (TypesGeography.StorableLine (VectorTile.Point x1 _) (VectorTile.Point x2 _)) = fromIntegral $ x2 - x1

deltaY :: TypesGeography.StorableLine -> Double
deltaY (TypesGeography.StorableLine (VectorTile.Point _ y1) (VectorTile.Point _ y2)) = fromIntegral $ y2 - y1
