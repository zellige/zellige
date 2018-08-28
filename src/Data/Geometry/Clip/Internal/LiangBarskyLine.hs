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
  case foldLine bb line dXdY of
    Nothing -> acc
    Just pt -> VectorStorable.cons (recreateLine pt line dXdY) acc
  where
    dXdY = dXAndDyFromLine line

foldLine :: TypesGeography.BoundingBox -> TypesGeography.StorableLine -> DxAndDy -> Maybe T1AndT2
foldLine bb line dXdY = Vector.foldl' (\acc edge -> calcT1AndT2OrQuit (calcPAndQ bb line dXdY edge) acc) newT1AndT2 (Vector.fromList [LeftEdge, RightEdge, BottomEdge, TopEdge])

data DxAndDy = DxAndDy !Double !Double
data T1AndT2 = T1AndT2 !Double !Double
data PAndQ = PAndQ !Double !Double

dXAndDyFromLine :: TypesGeography.StorableLine -> DxAndDy
dXAndDyFromLine (TypesGeography.StorableLine (VectorTile.Point x1 y1) (VectorTile.Point x2 y2)) = DxAndDy (fromIntegral $ x2 - x1) (fromIntegral $ y2 - y1)

newT1AndT2 :: Maybe T1AndT2
newT1AndT2 = Just (T1AndT2 0.0 1.0)

recreateLine :: T1AndT2 -> TypesGeography.StorableLine -> DxAndDy -> TypesGeography.StorableLine
recreateLine (T1AndT2 t1 t2) (TypesGeography.StorableLine (VectorTile.Point x1 y1) _) (DxAndDy dX dY) =
  TypesGeography.StorableLine
    (VectorTile.Point (round $ fromIntegral x1 + t1 * dX) (round $ fromIntegral y1 + t1 * dY))
    (VectorTile.Point (round $ fromIntegral x1 + t2 * dX) (round $ fromIntegral y1 + t2 * dY))

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

calcPAndQ :: TypesGeography.BoundingBox -> TypesGeography.StorableLine -> DxAndDy -> Edge -> PAndQ
calcPAndQ (TypesGeography.BoundingBox minX minY maxX maxY) (TypesGeography.StorableLine (VectorTile.Point x1 y1) _) (DxAndDy dX dY) e =
  case e of
    LeftEdge   -> PAndQ (-1 * dX) (fromIntegral x1 - minX)
    RightEdge  -> PAndQ dX (maxX - fromIntegral x1)
    BottomEdge -> PAndQ (-1 * dY) (fromIntegral y1 - minY)
    TopEdge    -> PAndQ dY (maxY - fromIntegral y1)
