{-# LANGUAGE FlexibleContexts #-}

-- TODO Change to linear ring.

module Data.Geometry.Clip.Internal.PolygonNichollLeeNicholl (
clipPolygonNLN
, clipPolygonsNLN
) where

import qualified Data.Aeson                       as Aeson
import qualified Data.Geometry.Clip.Internal.Line as ClipLine
import qualified Data.Geometry.Types.Geography    as TypesGeography
import qualified Data.Geospatial                  as Geospatial
import qualified Data.LinearRing                  as LinearRing
import qualified Data.List.NonEmpty               as ListNonEmpty
import qualified Data.Validation                  as Validation
import qualified Data.Vector                      as Vector
import qualified Data.Vector.Storable             as VectorStorable

clipPolygonsNLN :: TypesGeography.BoundingBox -> Geospatial.GeoMultiPolygon -> Geospatial.GeoFeature Aeson.Value -> Vector.Vector (Geospatial.GeoFeature Aeson.Value) -> Vector.Vector (Geospatial.GeoFeature Aeson.Value)
clipPolygonsNLN bb (Geospatial.GeoMultiPolygon polys) (Geospatial.GeoFeature bbox _ props fId) acc =
  case maybeNewMultiPoly bb polys of
    Nothing   -> acc
    Just newPolys -> Vector.cons (Geospatial.GeoFeature bbox (Geospatial.MultiPolygon (Geospatial.GeoMultiPolygon newPolys)) props fId) acc

maybeNewMultiPoly :: TypesGeography.BoundingBox -> Vector.Vector (Vector.Vector (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS)) -> Maybe (Vector.Vector (Vector.Vector (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS)))
maybeNewMultiPoly bb = traverse $ traverse (clipLinearRing bb)

clipPolygonNLN :: TypesGeography.BoundingBox -> Geospatial.GeoPolygon -> Geospatial.GeoFeature Aeson.Value -> Vector.Vector (Geospatial.GeoFeature Aeson.Value) -> Vector.Vector (Geospatial.GeoFeature Aeson.Value)
clipPolygonNLN bb (Geospatial.GeoPolygon poly) (Geospatial.GeoFeature bbox _ props fId) acc =
    case maybeNewPoly bb poly of
      Nothing   -> acc
      Just newPoly -> Vector.cons (Geospatial.GeoFeature bbox (Geospatial.Polygon (Geospatial.GeoPolygon newPoly)) props fId) acc

maybeNewPoly :: TypesGeography.BoundingBox -> Vector.Vector (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS) -> Maybe (Vector.Vector (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS))
maybeNewPoly bb = traverse (clipLinearRing bb)

clipLinearRing :: TypesGeography.BoundingBox -> LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS -> Maybe (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS)
clipLinearRing bb linearRing =
  case createNewClipPts of
    Nothing -> Nothing
    Just a ->
      case newLinearRing a of
        Validation.Failure _ -> Nothing
        Validation.Success b -> Just b
  where
    newLinearRing x = LinearRing.fromVector (VectorStorable.map Geospatial.GeoPointXY x) :: Validation.Validation (ListNonEmpty.NonEmpty (LinearRing.VectorToLinearRingError Geospatial.GeoPositionWithoutCRS)) (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS)
    createNewClipPts = clipPolyPoints bb (VectorStorable.map Geospatial.retrieveXY (LinearRing.toVector linearRing))

clipPolyPoints :: TypesGeography.BoundingBox -> VectorStorable.Vector Geospatial.PointXY -> Maybe (VectorStorable.Vector Geospatial.PointXY)
clipPolyPoints bb polyPoints =
  if VectorStorable.length newClippedPoly <= 2
  then Nothing
  else Just (closeIfNot newClippedPoly)
  where
    newClippedPoly = foo bb polyPoints

closeIfNot :: VectorStorable.Vector Geospatial.PointXY -> VectorStorable.Vector Geospatial.PointXY
closeIfNot poly =
  if lastPt /= firstPt
    then VectorStorable.cons lastPt poly
    else poly
  where
    lastPt = VectorStorable.last poly
    firstPt = VectorStorable.head poly

foo :: TypesGeography.BoundingBox-> VectorStorable.Vector Geospatial.PointXY -> VectorStorable.Vector Geospatial.PointXY
foo bb polyPts = if VectorStorable.length polyPts <= 2 then VectorStorable.empty else newPoints
  where
    newPoints = lineToClippedPoints bb (TypesGeography.pointsToLines polyPts)

lineToClippedPoints :: TypesGeography.BoundingBox -> VectorStorable.Vector TypesGeography.GeoStorableLine -> VectorStorable.Vector Geospatial.PointXY
lineToClippedPoints bb = VectorStorable.foldr (clipOrDiscard bb) VectorStorable.empty

clipOrDiscard :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine  -> VectorStorable.Vector Geospatial.PointXY -> VectorStorable.Vector Geospatial.PointXY
clipOrDiscard bb line acc =
  case foldLine bb line of
    Nothing          -> acc
    Just clippedLine -> (VectorStorable.++) clippedLine acc

-- Clip line to bounding box
-- Assumes y axis is pointing up
foldLine :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine -> Maybe (VectorStorable.Vector Geospatial.PointXY)
foldLine r = clipLine (reverseRectYAxis r)

clipLine :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine -> Maybe (VectorStorable.Vector Geospatial.PointXY)
clipLine r@(TypesGeography.BoundingBox left _ right _) l@(TypesGeography.GeoStorableLine  (Geospatial.PointXY p1x _) _)
  | p1x < left  = toPoints $ _p1Left r l
  | p1x > right = toPoints $ rotateLine180c <$> _p1Left (rotateRect180c r) (rotateLine180c l)
  | otherwise   = toPoints $ _p1Centre r l

toPoints :: Maybe TypesGeography.GeoStorableLine -> Maybe (VectorStorable.Vector Geospatial.PointXY)
toPoints a =
    case a of
        Just line -> Just $ ClipLine.pointsFromLine line
        Nothing   -> Nothing

makeLineFromSinglePoint :: Double -> Double -> TypesGeography.GeoStorableLine
makeLineFromSinglePoint a b =
  TypesGeography.GeoStorableLine  point point
  where
    point = Geospatial.PointXY a b

makeLine :: Double -> Double -> Double -> Double -> TypesGeography.GeoStorableLine
makeLine a b c d =
  TypesGeography.GeoStorableLine  point1 point2
  where
    point1 = Geospatial.PointXY a b
    point2 = Geospatial.PointXY c d

-- 1. "leftcolumn"
-- P1 is in one of the left regions
_p1Left :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine  -> Maybe TypesGeography.GeoStorableLine
_p1Left r@(TypesGeography.BoundingBox left top _ bottom) l@(TypesGeography.GeoStorableLine  (Geospatial.PointXY _ p1y) (Geospatial.PointXY p2x _))
  | p2x < left   = _p1Left_p2Left r l
  | p1y > top    = _p1LeftTop_p2NotLeft r l
  | p1y < bottom = reflectLineXAxis <$> _p1LeftTop_p2NotLeft (reflectRectXAxis r) (reflectLineXAxis l)
  | otherwise    = _p1LeftMiddle_p2NotLeft r l


-- Handle turning point if in left column
_p1Left_p2Left :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine  -> Maybe TypesGeography.GeoStorableLine
_p1Left_p2Left r@(TypesGeography.BoundingBox _ top _ bottom) l@(TypesGeography.GeoStorableLine  (Geospatial.PointXY _ p1y) (Geospatial.PointXY _ _))
    | p1y > top = _p1LeftTop_p2Left r l
    | p1y < bottom = _p1LeftBottom_p2Left r l
    | otherwise = _p1LeftMiddle_p2Left r l

_p1LeftTop_p2Left :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine  -> Maybe TypesGeography.GeoStorableLine
_p1LeftTop_p2Left (TypesGeography.BoundingBox left top _ bottom) (TypesGeography.GeoStorableLine  (Geospatial.PointXY _ _) (Geospatial.PointXY _ p2y))
    | p2y > top = Nothing
    | p2y < bottom = Just (makeLine left top left bottom)
    | otherwise = Just (makeLineFromSinglePoint left top)

_p1LeftBottom_p2Left :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine  -> Maybe TypesGeography.GeoStorableLine
_p1LeftBottom_p2Left (TypesGeography.BoundingBox left top _ bottom) (TypesGeography.GeoStorableLine  (Geospatial.PointXY _ _) (Geospatial.PointXY _ p2y))
    | p2y > top = Just (makeLine left bottom left top)
    | p2y < bottom = Nothing
    | otherwise = Just (makeLineFromSinglePoint left bottom)

_p1LeftMiddle_p2Left :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine  -> Maybe TypesGeography.GeoStorableLine
_p1LeftMiddle_p2Left (TypesGeography.BoundingBox left top _ bottom) (TypesGeography.GeoStorableLine  (Geospatial.PointXY _ _) (Geospatial.PointXY _ p2y))
    | p2y > top = Just (makeLineFromSinglePoint left top)
    | p2y < bottom = Just (makeLineFromSinglePoint left bottom)
    | otherwise = Nothing

-- 1.1. "topleftcorner"
-- P1 is in the left-top region, and P2 is not in any of the left regions
_p1LeftTop_p2NotLeft :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine  -> Maybe TypesGeography.GeoStorableLine
_p1LeftTop_p2NotLeft r@(TypesGeography.BoundingBox left top _ _) l@(TypesGeography.GeoStorableLine  _ (Geospatial.PointXY _ p2y))
  | p2y > top = Just (makeLineFromSinglePoint left top)
  | otherwise = _p1LeftTop_p2NotLeftTop r l d
  where
    d = delta l

-- P1 is in the left-top region, and P2 is not in any of the left or top regions
_p1LeftTop_p2NotLeftTop :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine  -> Geospatial.PointXY -> Maybe TypesGeography.GeoStorableLine
_p1LeftTop_p2NotLeftTop r l d
  | topP > leftP = _p1LeftTop_p2NotLeftTop' r l d leftP
  | otherwise    = reflectLineXMinusY <$> _p1LeftTop_p2NotLeftTop' (reflectRectXMinusY r) (reflectLineXMinusY l) (reflectPointXMinusY d) topP
  where
    topP  = topProduct r l d
    leftP = leftProduct r l d


-- 1.1.1. "leftbottomregion"
-- P1 is in the left-top region, and P2 is not in any of the left or top regions, and above the vector from P1 to the left-top corner
_p1LeftTop_p2NotLeftTop' :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine  -> Geospatial.PointXY -> Double -> Maybe TypesGeography.GeoStorableLine
_p1LeftTop_p2NotLeftTop' r@(TypesGeography.BoundingBox _ _ _ bottom) l@(TypesGeography.GeoStorableLine  _ (Geospatial.PointXY _ p2y)) d leftP
  | p2y < bottom = _p1LeftTop_p2Bottom r l d leftP
  | otherwise    = Just (TypesGeography.GeoStorableLine  (clipLeft r l d leftP) (_p1LeftTop_p2Middle r l d))

-- P1 is in the left-top region, and P2 is the centre-middle or right-middle region
_p1LeftTop_p2Middle :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine   -> Geospatial.PointXY -> Geospatial.PointXY
_p1LeftTop_p2Middle r@(TypesGeography.BoundingBox _ _ right _) l@(TypesGeography.GeoStorableLine  _ p2@(Geospatial.PointXY p2x _)) d
  | p2x > right = clipRight r l d rightP
  | otherwise   = p2
  where
    rightP = rightProduct r l d

-- P1 is in the left-top region, and P2 is in the centre-bottom or right-bottom region
_p1LeftTop_p2Bottom :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine   -> Geospatial.PointXY -> Double -> Maybe TypesGeography.GeoStorableLine
_p1LeftTop_p2Bottom r@(TypesGeography.BoundingBox left top _ bottom) l d leftP
  | bottomP > leftP = Just (makeLine left top left bottom)
  | otherwise       = Just (TypesGeography.GeoStorableLine  (clipLeft r l d leftP) (_p1LeftTop_p2Bottom' r l d bottomP))
  where
    bottomP = bottomProduct r l d

_p1LeftTop_p2Bottom' :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine   -> Geospatial.PointXY -> Double -> Geospatial.PointXY
_p1LeftTop_p2Bottom' r@(TypesGeography.BoundingBox _ _ right _) l@(TypesGeography.GeoStorableLine  _ (Geospatial.PointXY p2x _)) d bottomP
  | p2x > right = _p1LeftTop_p2BottomRight r l d bottomP
  | otherwise   = clipBottom r l d bottomP

-- P1 is in the left-top region, and P2 is in the right-bottom region
_p1LeftTop_p2BottomRight :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine   -> Geospatial.PointXY -> Double -> Geospatial.PointXY
_p1LeftTop_p2BottomRight r l d bottomP
  | bottomP > rightP = clipBottom r l d bottomP
  | otherwise        = clipRight r l d rightP
  where
    rightP = rightProduct r l d


-- 1.2. "leftedge"
-- P1 is in the left-middle region, and P2 is not in any of the left regions
_p1LeftMiddle_p2NotLeft :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine  -> Maybe TypesGeography.GeoStorableLine
_p1LeftMiddle_p2NotLeft r@(TypesGeography.BoundingBox _ top _ bottom) l@(TypesGeography.GeoStorableLine  _ (Geospatial.PointXY _ p2y))
  | p2y < bottom = _p1LeftMiddle_p2BottomNotLeft r l
  | p2y > top    = reflectLineXAxis <$> _p1LeftMiddle_p2BottomNotLeft (reflectRectXAxis r) (reflectLineXAxis l)
  | otherwise    = Just (TypesGeography.GeoStorableLine  (clipLeft r l d leftP) (_p1LeftMiddle_p2MiddleNotLeft r l d))
  where
    d     = delta l
    leftP = leftProduct r l d

-- P1 is in the left-middle region, and P2 is the centre-middle or right-middle region
_p1LeftMiddle_p2MiddleNotLeft :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine   -> Geospatial.PointXY -> Geospatial.PointXY
_p1LeftMiddle_p2MiddleNotLeft r@(TypesGeography.BoundingBox _ _ right _) l@(TypesGeography.GeoStorableLine  _ p2@(Geospatial.PointXY p2x _)) d
  | p2x > right = clipRight r l d rightP
  | otherwise   = p2
  where
    rightP = rightProduct r l d


-- 1.2.1. "p2bottom"
-- P1 is in the left-middle region, and P2 is in the centre-bottom or right-bottom region
_p1LeftMiddle_p2BottomNotLeft :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine  -> Maybe TypesGeography.GeoStorableLine
_p1LeftMiddle_p2BottomNotLeft r@(TypesGeography.BoundingBox left _ _ bottom) l
  | bottomP > leftP = Just (makeLineFromSinglePoint left bottom)
  | otherwise       = Just (TypesGeography.GeoStorableLine  (clipLeft r l d leftP) (_p1LeftMiddle_p2BottomNotLeft' r l d bottomP))
  where
    d       = delta l
    leftP   = leftProduct r l d
    bottomP = bottomProduct r l d

_p1LeftMiddle_p2BottomNotLeft' :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine   -> Geospatial.PointXY -> Double -> Geospatial.PointXY
_p1LeftMiddle_p2BottomNotLeft' r@(TypesGeography.BoundingBox _ _ right _) l@(TypesGeography.GeoStorableLine  _ (Geospatial.PointXY p2x _)) d bottomP
  | p2x > right = _p1LeftMiddle_p2RightBottom r l d bottomP
  | otherwise   = clipBottom r l d bottomP

-- P2 is beyond the right boundary
_p1LeftMiddle_p2RightBottom :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine  -> Geospatial.PointXY -> Double -> Geospatial.PointXY
_p1LeftMiddle_p2RightBottom r l d bottomP
  | bottomP > rightP = clipBottom r l d bottomP
  | otherwise        = clipRight r l d rightP
  where
    rightP = rightProduct r l d


-- 2. "centrecolumn"
-- P1 is in one of the centre regions
_p1Centre :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine  -> Maybe TypesGeography.GeoStorableLine
_p1Centre r@(TypesGeography.BoundingBox _ top _ bottom) l@(TypesGeography.GeoStorableLine  p1@(Geospatial.PointXY _ p1y) _)
  | p1y < bottom = _p1CentreBottom r l
  | p1y > top    = _p1CentreTop r l
  | otherwise    = Just (TypesGeography.GeoStorableLine  p1 ( _p1CentreMiddle r l))

-- P1 is in the centre-bottom region
_p1CentreBottom :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine  -> Maybe TypesGeography.GeoStorableLine
_p1CentreBottom r@(TypesGeography.BoundingBox _ _ _ bottom) l@(TypesGeography.GeoStorableLine  _ (Geospatial.PointXY _ p2y))
  | p2y < bottom = _p1CentreBottom_p2Bottom r l
  | otherwise    = rotateLine270c <$> _p1LeftMiddle_p2NotLeft (rotateRect90c r) (rotateLine90c l)

-- P1 is in the centre-top region
_p1CentreTop :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine  -> Maybe TypesGeography.GeoStorableLine
_p1CentreTop r@(TypesGeography.BoundingBox _ top _ _) l@(TypesGeography.GeoStorableLine  _ (Geospatial.PointXY _ p2y))
  | p2y > top = _p1CentreTop_p2Top r l
  | otherwise = rotateLine90c <$> _p1LeftMiddle_p2NotLeft (rotateRect270c r) (rotateLine270c l)

-- Handle turning point if in centre column
_p1CentreBottom_p2Bottom :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine  -> Maybe TypesGeography.GeoStorableLine
_p1CentreBottom_p2Bottom (TypesGeography.BoundingBox left _ right bottom) (TypesGeography.GeoStorableLine  (Geospatial.PointXY _ _) (Geospatial.PointXY p2x _))
    | p2x > right = Just $ makeLineFromSinglePoint right bottom
    | p2x < left = Just $ makeLineFromSinglePoint left bottom
    | otherwise = Nothing

_p1CentreTop_p2Top :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine  -> Maybe TypesGeography.GeoStorableLine
_p1CentreTop_p2Top (TypesGeography.BoundingBox left top right _) (TypesGeography.GeoStorableLine  (Geospatial.PointXY _ _) (Geospatial.PointXY p2x _))
    | p2x > right = Just $ makeLineFromSinglePoint right top
    | p2x < left = Just $ makeLineFromSinglePoint left top
    | otherwise = Nothing


-- 2.1. "inside"
-- P1 is in the centre-middle region
_p1CentreMiddle :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine  -> Geospatial.PointXY
_p1CentreMiddle r@(TypesGeography.BoundingBox left top right bottom) l@(TypesGeography.GeoStorableLine  _ p2@(Geospatial.PointXY p2x p2y))
  | p2x < left   = _p1CentreMiddle_p2Left r l
  | p2x > right  = rotatePoint180c $ _p1CentreMiddle_p2Left (rotateRect180c r) (rotateLine180c l)
  | p2y > top    = clipTop r l d topP
  | p2y < bottom = clipBottom r l d bottomP
  | otherwise    = p2
  where
    d       = delta l
    topP    = topProduct r l d
    bottomP = bottomProduct r l d

-- P1 is in the centre-middle region, and P2 is in one of the left regions
_p1CentreMiddle_p2Left :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine  -> Geospatial.PointXY
_p1CentreMiddle_p2Left r@(TypesGeography.BoundingBox _ top _ bottom) l@(TypesGeography.GeoStorableLine  _ (Geospatial.PointXY _ p2y))
  | p2y > top    = _p1CentreMiddle_p2LeftTop r l
  | p2y < bottom = rotatePoint270c $ _p1CentreMiddle_p2LeftTop (rotateRect90c r) (rotateLine90c l)
  | otherwise    = clipLeft r l d leftP
  where
    d     = delta l
    leftP = leftProduct r l d

-- P1 is in the centre-middle region, and P2 is in the left-top region
_p1CentreMiddle_p2LeftTop :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine  -> Geospatial.PointXY
_p1CentreMiddle_p2LeftTop r l
  | topP > leftP = clipTop r l d topP
  | otherwise    = clipLeft r l d leftP
  where
    d     = delta l
    leftP = leftProduct r l d
    topP  = topProduct r l d

delta :: TypesGeography.GeoStorableLine  -> Geospatial.PointXY
delta (TypesGeography.GeoStorableLine  (Geospatial.PointXY p1x p1y) (Geospatial.PointXY p2x p2y)) =
    Geospatial.PointXY (p2x - p1x) (p2y - p1y)

leftProduct :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine  -> Geospatial.PointXY -> Double
leftProduct (TypesGeography.BoundingBox left _ _ _) (TypesGeography.GeoStorableLine  (Geospatial.PointXY p1x _) _) (Geospatial.PointXY _ dy) =
  (left - p1x) * dy

topProduct :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine  -> Geospatial.PointXY -> Double
topProduct (TypesGeography.BoundingBox _ top _ _) (TypesGeography.GeoStorableLine  (Geospatial.PointXY _ p1y) _) (Geospatial.PointXY dx _) =
  (top - p1y) * dx

rightProduct :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine  -> Geospatial.PointXY -> Double
rightProduct (TypesGeography.BoundingBox _ _ right _) (TypesGeography.GeoStorableLine  (Geospatial.PointXY p1x _) _) (Geospatial.PointXY _ dy) =
  (right - p1x) * dy

bottomProduct :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine  -> Geospatial.PointXY -> Double
bottomProduct (TypesGeography.BoundingBox _ _ _ bottom) (TypesGeography.GeoStorableLine  (Geospatial.PointXY _ p1y) _) (Geospatial.PointXY dx _) =
  (bottom - p1y) * dx

clipLeft :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine  -> Geospatial.PointXY -> Double -> Geospatial.PointXY
clipLeft (TypesGeography.BoundingBox left _ _ _) (TypesGeography.GeoStorableLine  (Geospatial.PointXY _ p1y) _) (Geospatial.PointXY dx _) leftP =
    Geospatial.PointXY left (p1y + leftP / dx)

clipTop :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine  -> Geospatial.PointXY -> Double -> Geospatial.PointXY
clipTop (TypesGeography.BoundingBox _ top _ _) (TypesGeography.GeoStorableLine  (Geospatial.PointXY p1x _) _) (Geospatial.PointXY _ dy) topP =
    Geospatial.PointXY (p1x + topP / dy) top

clipRight :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine  -> Geospatial.PointXY -> Double -> Geospatial.PointXY
clipRight (TypesGeography.BoundingBox _ _ right _) (TypesGeography.GeoStorableLine  (Geospatial.PointXY _ p1y) _) (Geospatial.PointXY dx _) rightP =
    Geospatial.PointXY right (p1y + rightP / dx)

clipBottom :: TypesGeography.BoundingBox -> TypesGeography.GeoStorableLine  -> Geospatial.PointXY -> Double -> Geospatial.PointXY
clipBottom (TypesGeography.BoundingBox _ _ _ bottom) (TypesGeography.GeoStorableLine  (Geospatial.PointXY p1x _) _) (Geospatial.PointXY _ dy) bottomP =
    Geospatial.PointXY (p1x + bottomP / dy) bottom

rotateLine90c :: TypesGeography.GeoStorableLine  -> TypesGeography.GeoStorableLine
rotateLine90c (TypesGeography.GeoStorableLine  p1 p2) =
  TypesGeography.GeoStorableLine  (rotatePoint90c p1) (rotatePoint90c p2)

-- Rotate line 180° clockwise about the origin
rotateLine180c :: TypesGeography.GeoStorableLine  -> TypesGeography.GeoStorableLine
rotateLine180c (TypesGeography.GeoStorableLine  p1 p2) =
  TypesGeography.GeoStorableLine  (rotatePoint180c p1) (rotatePoint180c p2)

-- Rotate line 270° clockwise about the origin
rotateLine270c :: TypesGeography.GeoStorableLine  -> TypesGeography.GeoStorableLine
rotateLine270c (TypesGeography.GeoStorableLine  p1 p2) =
  TypesGeography.GeoStorableLine  (rotatePoint270c p1) (rotatePoint270c p2)

-- Reflect line about the line x = -y
reflectLineXMinusY :: TypesGeography.GeoStorableLine  -> TypesGeography.GeoStorableLine
reflectLineXMinusY (TypesGeography.GeoStorableLine  p1 p2) =
  TypesGeography.GeoStorableLine  (reflectPointXMinusY p1) (reflectPointXMinusY p2)

-- Reflect line about the x axis
reflectLineXAxis :: TypesGeography.GeoStorableLine  -> TypesGeography.GeoStorableLine
reflectLineXAxis (TypesGeography.GeoStorableLine  p1 p2) =
  TypesGeography.GeoStorableLine  (reflectPointXAxis p1) (reflectPointXAxis p2)


-- Rotate point 90° clockwise about the origin
rotatePoint90c :: Geospatial.PointXY -> Geospatial.PointXY
rotatePoint90c (Geospatial.PointXY x y) =
  Geospatial.PointXY y (-x)

-- Rotate point 180° clockwise about the origin
rotatePoint180c :: Geospatial.PointXY -> Geospatial.PointXY
rotatePoint180c (Geospatial.PointXY x y) =
    Geospatial.PointXY (-x) (-y)

-- Rotate point 270° clockwise about the origin
rotatePoint270c :: Geospatial.PointXY -> Geospatial.PointXY
rotatePoint270c (Geospatial.PointXY x y) =
  Geospatial.PointXY (-y) x

-- Reflect point about the line x = -y
reflectPointXMinusY :: Geospatial.PointXY -> Geospatial.PointXY
reflectPointXMinusY (Geospatial.PointXY x y) =
  Geospatial.PointXY (-y) (-x)

-- Reflect point about the x axis
reflectPointXAxis :: Geospatial.PointXY -> Geospatial.PointXY
reflectPointXAxis (Geospatial.PointXY x y) =
  Geospatial.PointXY x (-y)

-- Rotate rect 90° clockwise about the origin
rotateRect90c :: TypesGeography.BoundingBox-> TypesGeography.BoundingBox
rotateRect90c (TypesGeography.BoundingBox left top right bottom) =
  TypesGeography.BoundingBox bottom (-left) top (-right)

-- Rotate rect 180° clockwise about the origin
rotateRect180c :: TypesGeography.BoundingBox-> TypesGeography.BoundingBox
rotateRect180c (TypesGeography.BoundingBox left top right bottom) =
  TypesGeography.BoundingBox (-right) (-bottom) (-left) (-top)

-- Rotate rect 270° clockwise about the origin
rotateRect270c :: TypesGeography.BoundingBox-> TypesGeography.BoundingBox
rotateRect270c (TypesGeography.BoundingBox left top right bottom) =
  TypesGeography.BoundingBox (-top) right (-bottom) left

-- Reflect rect about the line x = -y
reflectRectXMinusY :: TypesGeography.BoundingBox-> TypesGeography.BoundingBox
reflectRectXMinusY (TypesGeography.BoundingBox left top right bottom) =
  TypesGeography.BoundingBox (-top) (-left) (-bottom) (-right)

-- Reflect rect about the x axis
reflectRectXAxis :: TypesGeography.BoundingBox-> TypesGeography.BoundingBox
reflectRectXAxis (TypesGeography.BoundingBox left top right bottom) =
  TypesGeography.BoundingBox left (-bottom) right (-top)

-- Reverse the direction of the y axis in rect
reverseRectYAxis :: TypesGeography.BoundingBox-> TypesGeography.BoundingBox
reverseRectYAxis (TypesGeography.BoundingBox left top right bottom) =
  TypesGeography.BoundingBox left bottom right top
