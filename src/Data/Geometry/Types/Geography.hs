{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Geometry.Types.Geography where

import qualified Data.Aeson           as Aeson
import qualified Data.Aeson.Types     as AesonTypes
import qualified Data.Geospatial      as Geospatial
import qualified Data.Scientific      as Scientific
import qualified Data.SeqHelper       as SeqHelper
import qualified Data.Sequence        as Sequence
import qualified Data.Vector          as Vector
import qualified Data.Word            as DataWord
import qualified Data.Geometry.VectorTile.VectorTile as VectorTile
import           Numeric.Natural      (Natural)
import           Prelude              hiding (Left, Right)

defaultVersion :: DataWord.Word
defaultVersion = 2

-- Pixels
type Pixels = Natural

defaultBuffer :: Pixels
defaultBuffer = 128

-- BoundingBox

data BoundingBox = BoundingBox
  { _bbMinX :: !Double
  , _bbMinY :: !Double
  , _bbMaxX :: !Double
  , _bbMaxY :: !Double
  } deriving (Show, Eq)

instance Aeson.ToJSON BoundingBox where
  toJSON BoundingBox{..} = Aeson.Array (Vector.fromList $ fmap (Aeson.Number . Scientific.fromFloatDigits) [_bbMinX, _bbMinY, _bbMaxX, _bbMaxY])

instance Aeson.FromJSON BoundingBox where
  parseJSON json = do
    [bbMinX, bbMinY, bbMaxX, bbMaxY] <- AesonTypes.parseJSON json
    return $ BoundingBox bbMinX bbMinY bbMaxX bbMaxY

data BoundingBoxPts = BoundingBoxPts
  { _bbMinPts :: VectorTile.Point
  , _bbMaxPts :: VectorTile.Point
  } deriving (Show, Eq)

data BoundingBoxRect = BoundingBoxRect
  { _left   :: !Int
  , _top    :: !Int
  , _right  :: !Int
  , _bottom :: !Int
  } deriving (Show, Eq)

bboxPtsToBbox :: BoundingBoxPts -> BoundingBox
bboxPtsToBbox (BoundingBoxPts (VectorTile.Point minX minY) (VectorTile.Point maxX maxY)) = BoundingBox (fromIntegral minX) (fromIntegral minY) (fromIntegral maxX) (fromIntegral maxY)

bboxPtsToBboxRect :: BoundingBoxPts -> BoundingBoxRect
bboxPtsToBboxRect (BoundingBoxPts (VectorTile.Point minX minY) (VectorTile.Point maxX maxY)) = BoundingBoxRect minX minY maxX maxY

mkBBoxPoly :: BoundingBox -> Sequence.Seq GeoStorableLine
mkBBoxPoly (BoundingBox x1 y1 x2 y2) = pointsToLines $ Sequence.fromList [Geospatial.PointXY x1 y1, Geospatial.PointXY x2 y1, Geospatial.PointXY x2 y2, Geospatial.PointXY x1 y2]

pointsToLines :: Sequence.Seq Geospatial.PointXY -> Sequence.Seq GeoStorableLine
pointsToLines pts@(_ Sequence.:<| (_ Sequence.:|> lastS)) = (Sequence.zipWith GeoStorableLine <*> SeqHelper.sequenceTail) $ lastS Sequence.<| pts
pointsToLines _ = Sequence.empty

-- Coords types

data LatLon = LatLon
  { _llLat :: Double
  , _llLon :: Double }

type ZoomLevel = Natural

type ZoomLevelInt = Int

data CoordsInt = CoordsInt
  { _coordsiX :: Int
  , _coordsiY :: Int
  } deriving (Eq, Show)

data GoogleTileCoordsInt = GoogleTileCoordsInt
  { _gtciZoom   :: ZoomLevelInt
  , _gtciCoords :: CoordsInt
  } deriving (Eq, Show)

mkGoogleTileCoordsInt :: Pixels -> Pixels -> Pixels -> GoogleTileCoordsInt
mkGoogleTileCoordsInt z x y = GoogleTileCoordsInt (toInt z) (CoordsInt (toInt x) (toInt y))

toInt :: Natural -> Int
toInt x = fromIntegral x :: Int

data OutCode = Inside
  | Left
  | Right
  | Bottom
  | Top
  deriving (Eq, Ord, Enum, Bounded, Read, Show)

outCodeToWord8 :: OutCode -> DataWord.Word8
outCodeToWord8 c =
    case c of
      Inside -> 0
      Left   -> 1
      Right  -> 2
      Bottom -> 4
      Top    -> 8

word8ToOutCode :: DataWord.Word8 -> OutCode
word8ToOutCode w =
    case w of
      0 -> Inside
      1 -> Left
      2 -> Right
      4 -> Bottom
      _ -> Top

data GeoClipPoint = GeoClipPoint
  { _geoClipPointCode  :: !OutCode
  , _geoClipPointPoint :: !Geospatial.PointXY
  } deriving (Eq, Show)

data GeoClipLine = GeoClipLine
  { _geoClipLine1 :: !GeoClipPoint
  , _geoClipLine2 :: !GeoClipPoint
  } deriving (Eq, Show)

data GeoStorableLine = GeoStorableLine
  { _geoStorableLinePt1 :: !Geospatial.PointXY
  , _geoStorableLinePt2 :: !Geospatial.PointXY
  } deriving (Eq, Show)

data StorableLine = StorableLine
  { _storableLinePt1 :: !VectorTile.Point
  , _storableLinePt2 :: !VectorTile.Point
  } deriving (Eq, Show)
