{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Geometry.Types.Types where

import qualified Control.Applicative as AP
import qualified Control.Monad.ST    as ST
import           Data.Monoid
import           Data.Text
import           Data.Word
import           Numeric.Natural     (Natural)
import           Options.Generic
import           Prelude             hiding (Left, Right)

-- Remove me in 8.4.1 https://ghc.haskell.org/trac/ghc/ticket/14107
instance Monoid a => Monoid (ST.ST s a) where
    mempty = pure mempty
    mappend = AP.liftA2 mappend

defaultVersion :: Int
defaultVersion = 2

defaultBuffer :: Pixels
defaultBuffer = Pixels 128

newtype Pixels = Pixels
  { _pixels :: Int
  } deriving (Show, Eq, Num)

mkConfig :: Text -> ZoomLevel -> (Integer, Integer) -> Pixels -> Pixels -> Config
mkConfig name z (x, y) buffer extents = Config name (GoogleTileCoords z (Coords x y)) buffer extents defaultVersion

data Config = Config
  { _name    :: Text
  , _gtc     :: GoogleTileCoords
  , _buffer  :: Pixels
  , _extents :: Pixels
  , _version :: Int
  } deriving (Show, Eq)

data BoundingBox = BoundingBox
  { _bbMinX :: Double
  , _bbMinY :: Double
  , _bbMaxX :: Double
  , _bbMaxY :: Double
  } deriving (Show, Eq)

data LatLon = LatLon
  { _llLat :: Double
  , _llLon :: Double }

type ZoomLevel = Natural

data GoogleTileCoords = GoogleTileCoords
  { _gtcZoom   :: ZoomLevel
  , _gtcCoords :: Coords
  } deriving (Eq, Show)

mkGoogleTileCoords :: ZoomLevel -> Integer -> Integer -> GoogleTileCoords
mkGoogleTileCoords z x y = GoogleTileCoords z (Coords x y)

data Coords = Coords
  { _coordsX :: Integer
  , _coordsY :: Integer
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

data LayerConfig w = LayerConfig
  { _layerInput  :: w ::: FilePath <?> "Input GeoJSON file"
  , _layerOutput :: w ::: FilePath <?> "Output Mapnik Vector Tile file"
  , _layerName   :: w ::: Text <?> "Name of layer"
  , _layerZoom   :: w ::: ZoomLevel <?> "Zoom level of layer"
  , _layerX      :: w ::: Integer <?> "Longitude of layer"
  , _layerY      :: w ::: Integer <?> "Latitude of layer"
  , _layerBuffer :: w ::: Int <?> "Buffer in pixels"
  , _layerExtent :: w ::: Int <?> "Layer size in pixels"
  } deriving (Generic)


modifiers :: Modifiers
modifiers = lispCaseModifiers

instance ParseRecord (LayerConfig Wrapped) where
  parseRecord = parseRecordWithModifiers modifiers

deriving instance Show (LayerConfig Unwrapped)

configFromLayerConfig :: LayerConfig Unwrapped -> Config
configFromLayerConfig lc = mkConfig (_layerName lc) (_layerZoom lc) (_layerX lc, _layerY lc) (Pixels $ _layerBuffer lc) (Pixels $ _layerExtent lc)
