{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Types where

import           Data.Monoid
import           Data.Vector.Unboxed.Deriving
import           Data.Word
import           Prelude                      hiding (Left, Right)

mvtExtents :: Integer
mvtExtents = 2048 :: Integer

gtc :: GoogleTileCoords
gtc = GoogleTileCoords mvtX mvtY defZoom
  where
    defZoom = 15
    mvtX = 28999
    mvtY = 19781

data BoundingBox = BoundingBox
  { _bbMinX :: Double
  , _bbMinY :: Double
  , _bbMaxX :: Double
  , _bbMaxY :: Double
  } deriving (Show, Eq)

data LatLon = LatLon
  { _llLat :: Double
  , _llLon :: Double }

data GoogleTileCoords = GoogleTileCoords
  { _gtcX    :: Integer
  , _gtcY    :: Integer
  , _gtcZoom :: Integer
  } deriving (Eq, Show)

data Options = Options
  { oVersion :: Last Int
  , oName    :: Last String
  , oExtent  :: Last Int
  } deriving (Show, Eq)

instance Monoid Options where
  mempty = Options mempty mempty mempty
  mappend x y = Options
    { oVersion = oVersion x <> oVersion y
    , oName    = oName    x <> oName    y
    , oExtent  = oExtent  x <> oExtent  y
    }

data OutCode = Inside | Left | Right | Bottom | Top deriving (Eq, Ord, Enum, Bounded, Read, Show)

outCodeToWord8 :: OutCode -> Word8
outCodeToWord8 c =
    case c of
      Inside -> 0
      Left -> 1
      Right -> 2
      Bottom -> 4
      Top -> 8

word8ToOutCode :: Word8 -> OutCode
word8ToOutCode w =
    case w of
      0 -> Inside
      1 -> Left
      2 -> Right
      4 -> Bottom
      _ -> Top

derivingUnbox "OutCode"
  [t| OutCode -> Word8 |]
  [| outCodeToWord8 |]
  [| word8ToOutCode |]


