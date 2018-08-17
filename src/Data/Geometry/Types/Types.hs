{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Geometry.Types.Types where

import qualified Data.ByteString.Lazy         as LBS
import qualified Data.Monoid                  as M
import qualified Data.Text                    as DT
import qualified Data.Text.Encoding           as DTE
import qualified Data.Vector.Storable         as VectorStorable
import           Data.Vector.Unboxed.Deriving
import qualified Data.Word                    as DW
import           Foreign.Storable
import qualified Geography.VectorTile         as VectorTile
import           Numeric.Natural              (Natural)
import           Prelude                      hiding (Left, Right)

import qualified Data.Geometry.Types.Simplify as DGTS

defaultVersion :: DW.Word
defaultVersion = 2

-- Pixels
type Pixels = Natural

defaultBuffer :: Pixels
defaultBuffer = 128

-- BoundingBox

data BoundingBox = BoundingBox
  { _bbMinX :: Double
  , _bbMinY :: Double
  , _bbMaxX :: Double
  , _bbMaxY :: Double
  } deriving (Show, Eq)

data BoundingBoxPts = BoundingBoxPts
  {
    _bbMinPts :: VectorTile.Point
  , _bbMaxPts :: VectorTile.Point
  } deriving (Show, Eq)

mkBBoxPoly :: BoundingBoxPts -> VectorStorable.Vector (VectorTile.Point, VectorTile.Point)
mkBBoxPoly BoundingBoxPts{_bbMinPts = (VectorTile.Point x1 y1), _bbMaxPts = (VectorTile.Point x2 y2)} = pointsToLines $ VectorStorable.fromList [VectorTile.Point x1 y1, VectorTile.Point x2 y1, VectorTile.Point x2 y2, VectorTile.Point x1 y2]

pointsToLines :: VectorStorable.Vector VectorTile.Point -> VectorStorable.Vector (VectorTile.Point, VectorTile.Point)
pointsToLines pts = (VectorStorable.zipWith (,) <*> VectorStorable.tail) $ VectorStorable.cons (VectorStorable.last pts) pts

-- Config

data Config = Config
  { _name           :: LBS.ByteString
  , _gtc            :: GoogleTileCoordsInt
  , _buffer         :: DW.Word
  , _extents        :: Int
  , _quantizePixels :: Int
  , _simplify       :: DGTS.SimplificationAlgorithm
  , _version        :: DW.Word
  } deriving (Show, Eq)

mkConfig :: DT.Text -> Pixels -> (Pixels, Pixels) -> Pixels -> Pixels -> Pixels -> DGTS.SimplificationAlgorithm -> Config
mkConfig name z (x, y) buffer extents quantizePixels simplify = Config ((LBS.fromStrict . DTE.encodeUtf8) name) (mkGoogleTileCoordsInt z x y) (fromIntegral buffer) (fromIntegral extents) (toInt quantizePixels) simplify defaultVersion

toInt :: Natural -> Int
toInt x = fromIntegral x :: Int

-- Zoom Config

data ZoomConfig = ZoomConfig
  { _zcExtents  :: Int
  , _zcQuantize :: Int
  , _zcBBox     :: BoundingBox
  , _zcSimplify :: DGTS.SimplificationAlgorithm
  } deriving (Eq, Show)

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

-- Options

data Options = Options
  { oVersion :: M.Last Int
  , oName    :: M.Last String
  , oExtent  :: M.Last Int
  } deriving (Show, Eq)

instance Monoid Options where
  mempty = Options mempty mempty mempty
  mappend x y = Options
    { oVersion = oVersion x M.<> oVersion y
    , oName    = oName    x M.<> oName    y
    , oExtent  = oExtent  x M.<> oExtent  y
    }

-- Outcode

data OutCode = Inside
  | Left
  | Right
  | Bottom
  | Top
  deriving (Eq, Ord, Enum, Bounded, Read, Show)

outCodeToWord8 :: OutCode -> DW.Word8
outCodeToWord8 c =
    case c of
      Inside -> 0
      Left   -> 1
      Right  -> 2
      Bottom -> 4
      Top    -> 8

word8ToOutCode :: DW.Word8 -> OutCode
word8ToOutCode w =
    case w of
      0 -> Inside
      1 -> Left
      2 -> Right
      4 -> Bottom
      _ -> Top

derivingUnbox "OutCode"
   [t| OutCode -> DW.Word8 |]
   [| outCodeToWord8 |]
   [| word8ToOutCode |]

instance VectorStorable.Storable  ((OutCode, VectorTile.Point), (OutCode, VectorTile.Point)) where
  sizeOf _ = (16 * 2) + (1 * 2)
  alignment _ = 8 * 2 + 2
  peek p = do
    p1 <- VectorTile.Point <$> peekByteOff p 0 <*> peekByteOff p 8
    p2 <- VectorTile.Point <$> peekByteOff p 16 <*> peekByteOff p 24
    o1 <- peekByteOff p 32
    o2 <- peekByteOff p 33
    pure ((word8ToOutCode o1, p1), (word8ToOutCode o2, p2))
  poke p ((o1, VectorTile.Point a1 b1), (o2, VectorTile.Point a2 b2)) = pokeByteOff p 0 a1 *> pokeByteOff p 8 b1 *> pokeByteOff p 16 a2 *> pokeByteOff p 24 b2 *> pokeByteOff p 32 (outCodeToWord8 o1) *> pokeByteOff p 33 (outCodeToWord8 o2)

instance VectorStorable.Storable (VectorTile.Point, VectorTile.Point) where
  sizeOf _ = 16 * 2
  alignment _ = 8 * 2
  peek p = do
    p1 <- VectorTile.Point <$> peekByteOff p 0 <*> peekByteOff p 8
    p2 <- VectorTile.Point <$> peekByteOff p 16 <*> peekByteOff p 24
    pure (p1, p2)
  poke p (VectorTile.Point a1 b1, VectorTile.Point a2 b2) = pokeByteOff p 0 a1 *> pokeByteOff p 8 b1 *> pokeByteOff p 16 a2 *> pokeByteOff p 24 b2
