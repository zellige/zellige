{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Geometry.Types where

import qualified Control.Applicative             as AP
import qualified Control.Monad.ST                as ST
import qualified Data.Aeson                      as A
import qualified Data.HashMap.Strict             as HM
import qualified Data.Map.Lazy                   as DMZ
import qualified Data.Maybe                      as M
import           Data.Monoid
import qualified Data.Scientific                 as S
import           Data.Text
import qualified Data.Text                       as T (Text)
import qualified Data.Vector                     as DV
import           Data.Vector.Unboxed.Deriving
import           Data.Word
import qualified Geography.VectorTile.Geometry   as VG
import qualified Geography.VectorTile.VectorTile as VT
import           Options.Generic
import           Prelude                         hiding (Left, Right)

-- Remove me in 8.4.1 https://ghc.haskell.org/trac/ghc/ticket/14107
instance Monoid a => Monoid (ST.ST s a) where
    mempty = pure mempty
    mappend = AP.liftA2 mappend

defaultVersion :: Int
defaultVersion = 2

defaultBuffer :: Pixels
defaultBuffer = Pixels 128

newtype Pixels = Pixels {_pixels :: Int} deriving (Show, Eq, Num)

mkConfig :: Text -> Integer -> (Integer, Integer) -> Pixels -> Pixels -> Config
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

data GoogleTileCoords = GoogleTileCoords
  { _gtcZoom   :: Integer
  , _gtcCoords :: Coords
  } deriving (Eq, Show)

data Coords = Coords
  { _coordsX :: Integer
  , _coordsY :: Integer
  } deriving (Eq, Show)

data Options = Options
  { oVersion :: Last Int
  , oName    :: Last String
  , oExtent  :: Last Int
  } deriving (Show, Eq)

data MvtFeatures = MvtFeatures
  { mvtPoints   :: !(DV.Vector (VT.Feature VG.Point))
  , mvtLines    :: !(DV.Vector (VT.Feature VG.LineString))
  , mvtPolygons :: !(DV.Vector (VT.Feature VG.Polygon))
  } deriving (Show, Eq)

mkPoint :: Int -> A.Value -> DV.Vector VG.Point -> MvtFeatures
mkPoint x props p = MvtFeatures (mkFeature x props p) mempty mempty

mkLineString :: Int -> A.Value -> DV.Vector VG.LineString -> MvtFeatures
mkLineString x props l = MvtFeatures mempty (mkFeature x props l) mempty

mkPolygon :: Int -> A.Value -> DV.Vector VG.Polygon -> MvtFeatures
mkPolygon x props o = MvtFeatures mempty mempty (mkFeature x props o)

mkFeature :: Int -> A.Value -> DV.Vector g -> DV.Vector (VT.Feature g)
mkFeature x props geoms = DV.singleton $ VT.Feature x (convertProps props) geoms

convertProps :: A.Value -> DMZ.Map T.Text VT.Val
convertProps (A.Object x) = DMZ.fromList . M.catMaybes $ Prelude.fmap convertElems (HM.toList x)
convertProps _ = DMZ.empty

convertElems :: (t, A.Value) -> Maybe (t, VT.Val)
convertElems (k, A.String v) = Just (k, VT.St v)
convertElems (k, A.Number v) = Just (k, VT.Do (sToF v))
convertElems (k, A.Bool v)   = Just (k, VT.B v)
convertElems _               = Nothing

sToF :: S.Scientific -> Double
sToF = S.toRealFloat


instance Monoid MvtFeatures where
  mempty = MvtFeatures mempty mempty mempty
  mappend a b = MvtFeatures ((mvtPoints a) DV.++ (mvtPoints b)) ((mvtLines a) DV.++ (mvtLines b)) ((mvtPolygons a) DV.++ (mvtPolygons b))

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

derivingUnbox "OutCode"
  [t| OutCode -> Word8 |]
  [| outCodeToWord8 |]
  [| word8ToOutCode |]

data LayerConfig w = LayerConfig
  { _layerInput  :: w ::: FilePath <?> "Input GeoJSON file"
  , _layerOutput :: w ::: FilePath <?> "Output Mapnik Vector Tile file"
  , _layerName   :: w ::: Text <?> "Name of layer"
  , _layerZoom   :: w ::: Integer <?> "Zoom level of layer"
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

