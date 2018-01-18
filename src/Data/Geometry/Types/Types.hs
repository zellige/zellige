{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Geometry.Types.Types where

import qualified Control.Applicative as AP
import qualified Control.Monad.ST    as ST
import           Data.Monoid
import           Data.Text
import           Data.Word
import           Numeric.Natural     (Natural)
import           Prelude             hiding (Left, Right)

-- Remove me in 8.4.1 https://ghc.haskell.org/trac/ghc/ticket/14107
instance Monoid a => Monoid (ST.ST s a) where
    mempty = pure mempty
    mappend = AP.liftA2 mappend

defaultVersion :: Int
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

-- Config

data Config = Config
  { _name           :: Text
  , _gtc            :: GoogleTileCoordsInt
  , _buffer         :: Int
  , _extents        :: Int
  , _quantizePixels :: Int
  , _version        :: Int
  } deriving (Show, Eq)

mkConfig :: Text -> Pixels -> (Pixels, Pixels) -> Pixels -> Pixels -> Pixels -> Config
mkConfig name z (x, y) buffer extents quantizePixels = Config name (mkGoogleTileCoordsInt z x y) (toInt buffer) (toInt extents) (toInt quantizePixels) defaultVersion

toInt :: Natural -> Int
toInt x = (fromIntegral x :: Int)

-- Zoom Config

data ZoomConfig = ZoomConfig
  { _zcExtents  :: Int
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

-- Outcode

data OutCode = Inside
  | Left
  | Right
  | Bottom
  | Top
  deriving (Eq, Ord, Enum, Bounded, Read, Show)

outCodeToWord8 :: OutCode -> Word8
outCodeToWord8 c =
    case c of
      Inside -> 0
      Left   -> 1
      Right  -> 2
      Bottom -> 4
      Top    -> 8

word8ToOutCode :: Word8 -> OutCode
word8ToOutCode w =
    case w of
      0 -> Inside
      1 -> Left
      2 -> Right
      4 -> Bottom
      _ -> Top
