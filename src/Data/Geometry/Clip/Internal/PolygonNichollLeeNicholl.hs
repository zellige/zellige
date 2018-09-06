{-# LANGUAGE FlexibleContexts #-}

-- TODO Change to linear ring.

module Data.Geometry.Clip.Internal.PolygonNichollLeeNicholl (
clipPolygonNLN
, clipPolygonsNLN
) where

import qualified Data.Vector                   as Vector
import qualified Data.Vector.Storable          as VectorStorable
import qualified Geography.VectorTile          as VectorTile

import qualified Data.Geometry.Types.Geography as TypesGeography
import qualified Data.Geometry.Clip.Internal.Line as ClipLine
-- import qualified Data.Geometry.Clip.Internal.LineNichollLeeNicholl as ClipLine

clipPolygonsNLN :: TypesGeography.BoundingBoxPts -> Vector.Vector VectorTile.Polygon -> Vector.Vector VectorTile.Polygon
clipPolygonsNLN bb = Vector.foldl' addPoly mempty
  where
    addPoly acc f =
      case clipPolygonNLN bb f of
        Nothing -> acc
        Just x  -> Vector.cons x acc

clipPolygonNLN :: TypesGeography.BoundingBoxPts -> VectorTile.Polygon -> Maybe VectorTile.Polygon
clipPolygonNLN bb (VectorTile.Polygon polyPoints inner) =
  case clipPolyPoints bb polyPoints of
    Nothing -> Nothing
    Just x  -> Just (VectorTile.Polygon x (clipPolygonsNLN bb inner))

clipPolyPoints :: TypesGeography.BoundingBoxPts -> VectorStorable.Vector VectorTile.Point -> Maybe (VectorStorable.Vector VectorTile.Point)
clipPolyPoints bb polyPoints = checkLength (VectorStorable.uniq newClippedPoly)
  where
    newClippedPoly = foo (TypesGeography.bboxPtsToBboxRect bb) polyPoints
    checkLength newPoly =
      if VectorStorable.length newPoly <= 2
        then Nothing
        else Just (closeIfNot newPoly)

closeIfNot :: VectorStorable.Vector VectorTile.Point -> VectorStorable.Vector VectorTile.Point
closeIfNot poly =
  if lastPt /= firstPt
    then VectorStorable.cons lastPt poly
    else poly
  where
    lastPt = VectorStorable.last poly
    firstPt = VectorStorable.head poly

foo :: TypesGeography.BoundingBoxRect-> VectorStorable.Vector VectorTile.Point -> VectorStorable.Vector VectorTile.Point
foo bb polyPts = if VectorStorable.length polyPts <= 2 then VectorStorable.empty else newPoints
  where
    newPoints = lineToClippedPoints bb (TypesGeography.pointsToLines polyPts)

lineToClippedPoints :: TypesGeography.BoundingBoxRect -> VectorStorable.Vector TypesGeography.StorableLine-> VectorStorable.Vector VectorTile.Point
lineToClippedPoints bb = VectorStorable.foldr (clipOrDiscard bb) VectorStorable.empty

clipOrDiscard :: TypesGeography.BoundingBoxRect -> TypesGeography.StorableLine -> VectorStorable.Vector VectorTile.Point -> VectorStorable.Vector VectorTile.Point
clipOrDiscard bb line acc =
  case foldLine bb line of
    Nothing          -> acc
    Just clippedLine -> (VectorStorable.++) clippedLine acc

data Line = Line
  { _x1 :: !Double
  , _y1 :: !Double
  , _x2 :: !Double
  , _y2 :: !Double
  } deriving (Show, Eq)

-- Clip line to bounding box
-- Assumes y axis is pointing up
foldLine :: TypesGeography.BoundingBoxRect -> TypesGeography.StorableLine -> Maybe (VectorStorable.Vector VectorTile.Point)
foldLine r = clipLine (reverseRectYAxis r)

clipLine :: TypesGeography.BoundingBoxRect -> TypesGeography.StorableLine -> Maybe (VectorStorable.Vector VectorTile.Point)
clipLine r@(TypesGeography.BoundingBoxRect left _ right _) l@(TypesGeography.StorableLine (VectorTile.Point p1x _) _)
  | p1x < left  = toPoints $ _p1Left r l
  | p1x > right = toPoints $ rotateLine180c <$> _p1Left (rotateRect180c r) (rotateLine180c l)
  | otherwise   = toPoints $ _p1Centre r l

toPoints :: Maybe TypesGeography.StorableLine -> Maybe (VectorStorable.Vector VectorTile.Point)
toPoints a = 
    case a of
        Just line -> Just $ ClipLine.pointsFromLine line
        Nothing   -> Nothing


makeLineFromSinglePoint :: Int -> Int -> TypesGeography.StorableLine 
makeLineFromSinglePoint a b =
  TypesGeography.StorableLine point point
  where
    point = VectorTile.Point (fromIntegral a) (fromIntegral b)

makeLine :: Int -> Int -> Int -> Int -> TypesGeography.StorableLine 
makeLine a b c d =
  TypesGeography.StorableLine point1 point2
  where
    point1 = VectorTile.Point (fromIntegral a) (fromIntegral b)
    point2 = VectorTile.Point (fromIntegral c) (fromIntegral d)

-- 1. "leftcolumn"
-- P1 is in one of the left regions
_p1Left :: TypesGeography.BoundingBoxRect -> TypesGeography.StorableLine -> Maybe TypesGeography.StorableLine 
_p1Left r@(TypesGeography.BoundingBoxRect left top _ bottom) l@(TypesGeography.StorableLine (VectorTile.Point _ p1y) (VectorTile.Point p2x _))
  | p2x < left   = _p1Left_p2Left r l
  | p1y > top    = _p1LeftTop_p2NotLeft r l
  | p1y < bottom = reflectLineXAxis <$> _p1LeftTop_p2NotLeft (reflectRectXAxis r) (reflectLineXAxis l)
  | otherwise    = _p1LeftMiddle_p2NotLeft r l


-- Handle turning point if in left column
_p1Left_p2Left :: TypesGeography.BoundingBoxRect -> TypesGeography.StorableLine -> Maybe TypesGeography.StorableLine 
_p1Left_p2Left r@(TypesGeography.BoundingBoxRect _ top _ bottom) l@(TypesGeography.StorableLine (VectorTile.Point _ p1y) (VectorTile.Point _ _))
    | p1y > top = _p1LeftTop_p2Left r l
    | p1y < bottom = _p1LeftBottom_p2Left r l
    | otherwise = _p1LeftMiddle_p2Left r l

_p1LeftTop_p2Left :: TypesGeography.BoundingBoxRect -> TypesGeography.StorableLine -> Maybe TypesGeography.StorableLine 
_p1LeftTop_p2Left (TypesGeography.BoundingBoxRect left top _ bottom) (TypesGeography.StorableLine (VectorTile.Point _ _) (VectorTile.Point _ p2y))
    | p2y > top = Nothing
    | p2y < bottom = Just (makeLine left top left bottom)
    | otherwise = Just (makeLineFromSinglePoint left top)

_p1LeftBottom_p2Left :: TypesGeography.BoundingBoxRect -> TypesGeography.StorableLine -> Maybe TypesGeography.StorableLine 
_p1LeftBottom_p2Left (TypesGeography.BoundingBoxRect left top _ bottom) (TypesGeography.StorableLine (VectorTile.Point _ _) (VectorTile.Point _ p2y))
    | p2y > top = Just (makeLine left bottom left top)
    | p2y < bottom = Nothing
    | otherwise = Just (makeLineFromSinglePoint left bottom)

_p1LeftMiddle_p2Left :: TypesGeography.BoundingBoxRect -> TypesGeography.StorableLine -> Maybe TypesGeography.StorableLine 
_p1LeftMiddle_p2Left (TypesGeography.BoundingBoxRect left top _ bottom) (TypesGeography.StorableLine (VectorTile.Point _ _) (VectorTile.Point _ p2y))
    | p2y > top = Just (makeLineFromSinglePoint left top)
    | p2y < bottom = Just (makeLineFromSinglePoint left bottom)
    | otherwise = Nothing

-- 1.1. "topleftcorner"
-- P1 is in the left-top region, and P2 is not in any of the left regions
_p1LeftTop_p2NotLeft :: TypesGeography.BoundingBoxRect -> TypesGeography.StorableLine -> Maybe TypesGeography.StorableLine 
_p1LeftTop_p2NotLeft r@(TypesGeography.BoundingBoxRect left top _ _) l@(TypesGeography.StorableLine _ (VectorTile.Point _ p2y))
  | p2y > top = Just (makeLineFromSinglePoint left top)
  | otherwise = _p1LeftTop_p2NotLeftTop r l d
  where
    d = delta l

-- P1 is in the left-top region, and P2 is not in any of the left or top regions
_p1LeftTop_p2NotLeftTop :: TypesGeography.BoundingBoxRect -> TypesGeography.StorableLine -> VectorTile.Point -> Maybe TypesGeography.StorableLine 
_p1LeftTop_p2NotLeftTop r l d
  | topP > leftP = _p1LeftTop_p2NotLeftTop' r l d leftP
  | otherwise    = reflectLineXMinusY <$> _p1LeftTop_p2NotLeftTop' (reflectRectXMinusY r) (reflectLineXMinusY l) (reflectPointXMinusY d) topP
  where
    topP  = topProduct r l d
    leftP = leftProduct r l d


-- 1.1.1. "leftbottomregion"
-- P1 is in the left-top region, and P2 is not in any of the left or top regions, and above the vector from P1 to the left-top corner
_p1LeftTop_p2NotLeftTop' :: TypesGeography.BoundingBoxRect -> TypesGeography.StorableLine -> VectorTile.Point -> Int -> Maybe TypesGeography.StorableLine 
_p1LeftTop_p2NotLeftTop' r@(TypesGeography.BoundingBoxRect _ _ _ bottom) l@(TypesGeography.StorableLine _ (VectorTile.Point _ p2y)) d leftP
  | p2y < bottom = _p1LeftTop_p2Bottom r l d leftP
  | otherwise    = Just (TypesGeography.StorableLine (clipLeft r l d leftP) (_p1LeftTop_p2Middle r l d))

-- P1 is in the left-top region, and P2 is the centre-middle or right-middle region
_p1LeftTop_p2Middle :: TypesGeography.BoundingBoxRect -> TypesGeography.StorableLine  -> VectorTile.Point -> VectorTile.Point
_p1LeftTop_p2Middle r@(TypesGeography.BoundingBoxRect _ _ right _) l@(TypesGeography.StorableLine _ p2@(VectorTile.Point p2x _)) d
  | p2x > right = clipRight r l d rightP
  | otherwise   = p2
  where
    rightP = rightProduct r l d

-- P1 is in the left-top region, and P2 is in the centre-bottom or right-bottom region
_p1LeftTop_p2Bottom :: TypesGeography.BoundingBoxRect -> TypesGeography.StorableLine  -> VectorTile.Point -> Int -> Maybe TypesGeography.StorableLine 
_p1LeftTop_p2Bottom r@(TypesGeography.BoundingBoxRect left top _ bottom) l d leftP
  | bottomP > leftP = Just (makeLine left top left bottom)
  | otherwise       = Just (TypesGeography.StorableLine (clipLeft r l d leftP) (_p1LeftTop_p2Bottom' r l d bottomP))
  where
    bottomP = bottomProduct r l d

_p1LeftTop_p2Bottom' :: TypesGeography.BoundingBoxRect -> TypesGeography.StorableLine  -> VectorTile.Point -> Int -> VectorTile.Point
_p1LeftTop_p2Bottom' r@(TypesGeography.BoundingBoxRect _ _ right _) l@(TypesGeography.StorableLine _ (VectorTile.Point p2x _)) d bottomP
  | p2x > right = _p1LeftTop_p2BottomRight r l d bottomP
  | otherwise   = clipBottom r l d bottomP

-- P1 is in the left-top region, and P2 is in the right-bottom region
_p1LeftTop_p2BottomRight :: TypesGeography.BoundingBoxRect -> TypesGeography.StorableLine  -> VectorTile.Point -> Int -> VectorTile.Point
_p1LeftTop_p2BottomRight r l d bottomP
  | bottomP > rightP = clipBottom r l d bottomP
  | otherwise        = clipRight r l d rightP
  where
    rightP = rightProduct r l d


-- 1.2. "leftedge"
-- P1 is in the left-middle region, and P2 is not in any of the left regions
_p1LeftMiddle_p2NotLeft :: TypesGeography.BoundingBoxRect -> TypesGeography.StorableLine -> Maybe TypesGeography.StorableLine
_p1LeftMiddle_p2NotLeft r@(TypesGeography.BoundingBoxRect _ top _ bottom) l@(TypesGeography.StorableLine _ (VectorTile.Point _ p2y))
  | p2y < bottom = _p1LeftMiddle_p2BottomNotLeft r l
  | p2y > top    = reflectLineXAxis <$> _p1LeftMiddle_p2BottomNotLeft (reflectRectXAxis r) (reflectLineXAxis l)
  | otherwise    = Just (TypesGeography.StorableLine (clipLeft r l d leftP) (_p1LeftMiddle_p2MiddleNotLeft r l d))
  where
    d     = delta l
    leftP = leftProduct r l d

-- P1 is in the left-middle region, and P2 is the centre-middle or right-middle region
_p1LeftMiddle_p2MiddleNotLeft :: TypesGeography.BoundingBoxRect -> TypesGeography.StorableLine  -> VectorTile.Point -> VectorTile.Point
_p1LeftMiddle_p2MiddleNotLeft r@(TypesGeography.BoundingBoxRect _ _ right _) l@(TypesGeography.StorableLine _ p2@(VectorTile.Point p2x _)) d
  | p2x > right = clipRight r l d rightP
  | otherwise   = p2
  where
    rightP = rightProduct r l d


-- 1.2.1. "p2bottom"
-- P1 is in the left-middle region, and P2 is in the centre-bottom or right-bottom region
_p1LeftMiddle_p2BottomNotLeft :: TypesGeography.BoundingBoxRect -> TypesGeography.StorableLine -> Maybe TypesGeography.StorableLine
_p1LeftMiddle_p2BottomNotLeft r@(TypesGeography.BoundingBoxRect left _ _ bottom) l
  | bottomP > leftP = Just (makeLineFromSinglePoint left bottom)
  | otherwise       = Just (TypesGeography.StorableLine (clipLeft r l d leftP) (_p1LeftMiddle_p2BottomNotLeft' r l d bottomP))
  where
    d       = delta l
    leftP   = leftProduct r l d
    bottomP = bottomProduct r l d

_p1LeftMiddle_p2BottomNotLeft' :: TypesGeography.BoundingBoxRect -> TypesGeography.StorableLine  -> VectorTile.Point -> Int -> VectorTile.Point
_p1LeftMiddle_p2BottomNotLeft' r@(TypesGeography.BoundingBoxRect _ _ right _) l@(TypesGeography.StorableLine _ (VectorTile.Point p2x _)) d bottomP
  | p2x > right = _p1LeftMiddle_p2RightBottom r l d bottomP
  | otherwise   = clipBottom r l d bottomP

-- P2 is beyond the right boundary
_p1LeftMiddle_p2RightBottom :: TypesGeography.BoundingBoxRect -> TypesGeography.StorableLine -> VectorTile.Point -> Int -> VectorTile.Point
_p1LeftMiddle_p2RightBottom r l d bottomP
  | bottomP > rightP = clipBottom r l d bottomP
  | otherwise        = clipRight r l d rightP
  where
    rightP = rightProduct r l d


-- 2. "centrecolumn"
-- P1 is in one of the centre regions
_p1Centre :: TypesGeography.BoundingBoxRect -> TypesGeography.StorableLine -> Maybe TypesGeography.StorableLine
_p1Centre r@(TypesGeography.BoundingBoxRect _ top _ bottom) l@(TypesGeography.StorableLine p1@(VectorTile.Point _ p1y) _)
  | p1y < bottom = _p1CentreBottom r l
  | p1y > top    = _p1CentreTop r l
  | otherwise    = Just (TypesGeography.StorableLine p1 ( _p1CentreMiddle r l))

-- P1 is in the centre-bottom region
_p1CentreBottom :: TypesGeography.BoundingBoxRect -> TypesGeography.StorableLine -> Maybe TypesGeography.StorableLine 
_p1CentreBottom r@(TypesGeography.BoundingBoxRect _ _ _ bottom) l@(TypesGeography.StorableLine _ (VectorTile.Point _ p2y))
  | p2y < bottom = _p1CentreBottom_p2Bottom r l
  | otherwise    = rotateLine270c <$> _p1LeftMiddle_p2NotLeft (rotateRect90c r) (rotateLine90c l)

-- P1 is in the centre-top region
_p1CentreTop :: TypesGeography.BoundingBoxRect -> TypesGeography.StorableLine -> Maybe TypesGeography.StorableLine 
_p1CentreTop r@(TypesGeography.BoundingBoxRect _ top _ _) l@(TypesGeography.StorableLine _ (VectorTile.Point _ p2y))
  | p2y > top = _p1CentreTop_p2Top r l
  | otherwise = rotateLine90c <$> _p1LeftMiddle_p2NotLeft (rotateRect270c r) (rotateLine270c l)

-- Handle turning point if in centre column
_p1CentreBottom_p2Bottom :: TypesGeography.BoundingBoxRect -> TypesGeography.StorableLine -> Maybe TypesGeography.StorableLine 
_p1CentreBottom_p2Bottom (TypesGeography.BoundingBoxRect left _ right bottom) (TypesGeography.StorableLine (VectorTile.Point _ _) (VectorTile.Point p2x _))
    | p2x > right = Just $ makeLineFromSinglePoint right bottom
    | p2x < left = Just $ makeLineFromSinglePoint left bottom
    | otherwise = Nothing

_p1CentreTop_p2Top :: TypesGeography.BoundingBoxRect -> TypesGeography.StorableLine -> Maybe TypesGeography.StorableLine 
_p1CentreTop_p2Top (TypesGeography.BoundingBoxRect left top right _) (TypesGeography.StorableLine (VectorTile.Point _ _) (VectorTile.Point p2x _))
    | p2x > right = Just $ makeLineFromSinglePoint right top
    | p2x < left = Just $ makeLineFromSinglePoint left top
    | otherwise = Nothing


-- 2.1. "inside"
-- P1 is in the centre-middle region
_p1CentreMiddle :: TypesGeography.BoundingBoxRect -> TypesGeography.StorableLine -> VectorTile.Point
_p1CentreMiddle r@(TypesGeography.BoundingBoxRect left top right bottom) l@(TypesGeography.StorableLine _ p2@(VectorTile.Point p2x p2y))
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
_p1CentreMiddle_p2Left :: TypesGeography.BoundingBoxRect -> TypesGeography.StorableLine -> VectorTile.Point
_p1CentreMiddle_p2Left r@(TypesGeography.BoundingBoxRect _ top _ bottom) l@(TypesGeography.StorableLine _ (VectorTile.Point _ p2y))
  | p2y > top    = _p1CentreMiddle_p2LeftTop r l
  | p2y < bottom = rotatePoint270c $ _p1CentreMiddle_p2LeftTop (rotateRect90c r) (rotateLine90c l)
  | otherwise    = clipLeft r l d leftP
  where
    d     = delta l
    leftP = leftProduct r l d

-- P1 is in the centre-middle region, and P2 is in the left-top region
_p1CentreMiddle_p2LeftTop :: TypesGeography.BoundingBoxRect -> TypesGeography.StorableLine -> VectorTile.Point
_p1CentreMiddle_p2LeftTop r l
  | topP > leftP = clipTop r l d topP
  | otherwise    = clipLeft r l d leftP
  where
    d     = delta l
    leftP = leftProduct r l d
    topP  = topProduct r l d

delta :: TypesGeography.StorableLine -> VectorTile.Point
delta (TypesGeography.StorableLine (VectorTile.Point p1x p1y) (VectorTile.Point p2x p2y)) =
    VectorTile.Point (p2x - p1x) (p2y - p1y)

leftProduct :: TypesGeography.BoundingBoxRect -> TypesGeography.StorableLine -> VectorTile.Point -> Int
leftProduct (TypesGeography.BoundingBoxRect left _ _ _) (TypesGeography.StorableLine (VectorTile.Point p1x _) _) (VectorTile.Point _ dy) =
  (left - p1x) * dy

topProduct :: TypesGeography.BoundingBoxRect -> TypesGeography.StorableLine -> VectorTile.Point -> Int
topProduct (TypesGeography.BoundingBoxRect _ top _ _) (TypesGeography.StorableLine (VectorTile.Point _ p1y) _) (VectorTile.Point dx _) =
  (top - p1y) * dx

rightProduct :: TypesGeography.BoundingBoxRect -> TypesGeography.StorableLine -> VectorTile.Point -> Int
rightProduct (TypesGeography.BoundingBoxRect _ _ right _) (TypesGeography.StorableLine (VectorTile.Point p1x _) _) (VectorTile.Point _ dy) =
  (right - p1x) * dy

bottomProduct :: TypesGeography.BoundingBoxRect -> TypesGeography.StorableLine -> VectorTile.Point -> Int
bottomProduct (TypesGeography.BoundingBoxRect _ _ _ bottom) (TypesGeography.StorableLine (VectorTile.Point _ p1y) _) (VectorTile.Point dx _) =
  (bottom - p1y) * dx

clipLeft :: TypesGeography.BoundingBoxRect -> TypesGeography.StorableLine -> VectorTile.Point -> Int -> VectorTile.Point 
clipLeft (TypesGeography.BoundingBoxRect left _ _ _) (TypesGeography.StorableLine (VectorTile.Point _ p1y) _) (VectorTile.Point dx _) leftP =
    VectorTile.Point left (p1y + leftP `div` dx)

clipTop :: TypesGeography.BoundingBoxRect -> TypesGeography.StorableLine -> VectorTile.Point -> Int -> VectorTile.Point 
clipTop (TypesGeography.BoundingBoxRect _ top _ _) (TypesGeography.StorableLine (VectorTile.Point p1x _) _) (VectorTile.Point _ dy) topP =
    VectorTile.Point (p1x + topP `div` dy) top

clipRight :: TypesGeography.BoundingBoxRect -> TypesGeography.StorableLine -> VectorTile.Point -> Int -> VectorTile.Point 
clipRight (TypesGeography.BoundingBoxRect _ _ right _) (TypesGeography.StorableLine (VectorTile.Point _ p1y) _) (VectorTile.Point dx _) rightP =
    VectorTile.Point right (p1y + rightP `div` dx)

clipBottom :: TypesGeography.BoundingBoxRect -> TypesGeography.StorableLine -> VectorTile.Point -> Int -> VectorTile.Point 
clipBottom (TypesGeography.BoundingBoxRect _ _ _ bottom) (TypesGeography.StorableLine (VectorTile.Point p1x _) _) (VectorTile.Point _ dy) bottomP =
    VectorTile.Point (p1x + bottomP `div` dy) bottom

rotateLine90c :: TypesGeography.StorableLine -> TypesGeography.StorableLine
rotateLine90c (TypesGeography.StorableLine p1 p2) =
  TypesGeography.StorableLine (rotatePoint90c p1) (rotatePoint90c p2)

-- Rotate line 180° clockwise about the origin
rotateLine180c :: TypesGeography.StorableLine -> TypesGeography.StorableLine
rotateLine180c (TypesGeography.StorableLine p1 p2) =
  TypesGeography.StorableLine (rotatePoint180c p1) (rotatePoint180c p2)

-- Rotate line 270° clockwise about the origin
rotateLine270c :: TypesGeography.StorableLine -> TypesGeography.StorableLine
rotateLine270c (TypesGeography.StorableLine p1 p2) =
  TypesGeography.StorableLine (rotatePoint270c p1) (rotatePoint270c p2)

-- Reflect line about the line x = -y
reflectLineXMinusY :: TypesGeography.StorableLine -> TypesGeography.StorableLine
reflectLineXMinusY (TypesGeography.StorableLine p1 p2) =
  TypesGeography.StorableLine (reflectPointXMinusY p1) (reflectPointXMinusY p2)

-- Reflect line about the x axis
reflectLineXAxis :: TypesGeography.StorableLine -> TypesGeography.StorableLine
reflectLineXAxis (TypesGeography.StorableLine p1 p2) =
  TypesGeography.StorableLine (reflectPointXAxis p1) (reflectPointXAxis p2)


-- Rotate point 90° clockwise about the origin
rotatePoint90c :: VectorTile.Point -> VectorTile.Point
rotatePoint90c (VectorTile.Point x y) =
  VectorTile.Point y (-x)

-- Rotate point 180° clockwise about the origin
rotatePoint180c :: VectorTile.Point -> VectorTile.Point
rotatePoint180c (VectorTile.Point x y) =
    VectorTile.Point (-x) (-y)

-- Rotate point 270° clockwise about the origin
rotatePoint270c :: VectorTile.Point -> VectorTile.Point
rotatePoint270c (VectorTile.Point x y) =
  VectorTile.Point (-y) x

-- Reflect point about the line x = -y
reflectPointXMinusY :: VectorTile.Point -> VectorTile.Point
reflectPointXMinusY (VectorTile.Point x y) =
  VectorTile.Point (-y) (-x)

-- Reflect point about the x axis
reflectPointXAxis :: VectorTile.Point -> VectorTile.Point
reflectPointXAxis (VectorTile.Point x y) =
  VectorTile.Point x (-y)

-- Rotate rect 90° clockwise about the origin
rotateRect90c :: TypesGeography.BoundingBoxRect-> TypesGeography.BoundingBoxRect
rotateRect90c (TypesGeography.BoundingBoxRect left top right bottom) =
  TypesGeography.BoundingBoxRect bottom (-left) top (-right)

-- Rotate rect 180° clockwise about the origin
rotateRect180c :: TypesGeography.BoundingBoxRect-> TypesGeography.BoundingBoxRect
rotateRect180c (TypesGeography.BoundingBoxRect left top right bottom) =
  TypesGeography.BoundingBoxRect (-right) (-bottom) (-left) (-top)

-- Rotate rect 270° clockwise about the origin
rotateRect270c :: TypesGeography.BoundingBoxRect-> TypesGeography.BoundingBoxRect
rotateRect270c (TypesGeography.BoundingBoxRect left top right bottom) =
  TypesGeography.BoundingBoxRect (-top) right (-bottom) left

-- Reflect rect about the line x = -y
reflectRectXMinusY :: TypesGeography.BoundingBoxRect-> TypesGeography.BoundingBoxRect
reflectRectXMinusY (TypesGeography.BoundingBoxRect left top right bottom) =
  TypesGeography.BoundingBoxRect (-top) (-left) (-bottom) (-right)

-- Reflect rect about the x axis
reflectRectXAxis :: TypesGeography.BoundingBoxRect-> TypesGeography.BoundingBoxRect
reflectRectXAxis (TypesGeography.BoundingBoxRect left top right bottom) =
  TypesGeography.BoundingBoxRect left (-bottom) right (-top)

-- Reverse the direction of the y axis in rect
reverseRectYAxis :: TypesGeography.BoundingBoxRect-> TypesGeography.BoundingBoxRect
reverseRectYAxis (TypesGeography.BoundingBoxRect left top right bottom) =
  TypesGeography.BoundingBoxRect left bottom right top
