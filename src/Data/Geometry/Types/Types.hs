{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Geometry.Types.Types where

import qualified Control.Applicative          as AP
import qualified Control.Monad.ST             as ST
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.Monoid                  as M
import qualified Data.Text                    as DT
import qualified Data.Text.Encoding           as DTE
import qualified Data.Vector.Unboxed          as DVU
import           Data.Vector.Unboxed.Deriving
import qualified Data.Word                    as DW
import qualified Geography.VectorTile         as VG
import           Numeric.Natural              (Natural)
import           Prelude                      hiding (Left, Right)

-- Remove me in 8.4.1 https://ghc.haskell.org/trac/ghc/ticket/14107
instance Monoid a => Monoid (ST.ST s a) where
    mempty = pure mempty
    mappend = AP.liftA2 mappend

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
    _bbMinPts :: VG.Point
  , _bbMaxPts :: VG.Point
  } deriving (Show, Eq)

mkBBoxPoly :: BoundingBoxPts -> DVU.Vector (VG.Point, VG.Point)
mkBBoxPoly BoundingBoxPts{_bbMinPts = (x1, y1), _bbMaxPts = (x2, y2)} = pointsToLines $ DVU.fromList [(x1, y1), (x2, y1), (x2, y2), (x1, y2)]

pointsToLines :: DVU.Vector VG.Point -> DVU.Vector (VG.Point, VG.Point)
pointsToLines pts = (DVU.zip <*> DVU.tail) $ DVU.cons (DVU.last pts) pts

-- Config

data Config = Config
  { _name           :: LBS.ByteString
  , _gtc            :: GoogleTileCoordsInt
  , _buffer         :: DW.Word
  , _extents        :: DW.Word
  , _quantizePixels :: Int
  , _version        :: DW.Word
  } deriving (Show, Eq)

mkConfig :: DT.Text -> Pixels -> (Pixels, Pixels) -> Pixels -> Pixels -> Pixels -> Config
mkConfig name z (x, y) buffer extents quantizePixels = Config ((LBS.fromStrict . DTE.encodeUtf8) name) (mkGoogleTileCoordsInt z x y) (fromIntegral buffer) (fromIntegral extents) (toInt quantizePixels) defaultVersion

toInt :: Natural -> Int
toInt x = (fromIntegral x :: Int)

-- Zoom Config

data ZoomConfig = ZoomConfig
  { _zcExtents  :: DW.Word
  , _zcQuantize :: Int
  , _zcBBox     :: BoundingBox
  } deriving (Eq, Show)

-- Coords types

data LatLon = LatLon
  { _llLat :: Double
  , _llLon :: Double }

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
