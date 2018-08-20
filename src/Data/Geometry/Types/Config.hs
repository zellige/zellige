{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Geometry.Types.Config where

import qualified Data.ByteString.Lazy          as LBS
import qualified Data.Monoid                   as M
import qualified Data.Text                     as DT
import qualified Data.Text.Encoding            as DTE
import qualified Data.Word                     as DW

import qualified Data.Geometry.Types.Geography as TypesGeography
import qualified Data.Geometry.Types.Simplify  as TypesSimplfy

data Config = Config
  { _name           :: LBS.ByteString
  , _gtc            :: TypesGeography.GoogleTileCoordsInt
  , _buffer         :: DW.Word
  , _extents        :: Int
  , _quantizePixels :: Int
  , _simplify       :: TypesSimplfy.SimplificationAlgorithm
  , _version        :: DW.Word
  } deriving (Show, Eq)

mkConfig :: DT.Text -> TypesGeography.Pixels -> (TypesGeography.Pixels, TypesGeography.Pixels) -> TypesGeography.Pixels -> TypesGeography.Pixels -> TypesGeography.Pixels -> TypesSimplfy.SimplificationAlgorithm -> Config
mkConfig name z (x, y) buffer extents quantizePixels simplify = Config ((LBS.fromStrict . DTE.encodeUtf8) name) (TypesGeography.mkGoogleTileCoordsInt z x y)
  (fromIntegral buffer) (fromIntegral extents) (TypesGeography.toInt quantizePixels) simplify TypesGeography.defaultVersion

-- Zoom Config

data ZoomConfig = ZoomConfig
  { _zcExtents  :: Int
  , _zcQuantize :: Int
  , _zcBBox     :: TypesGeography.BoundingBox
  , _zcSimplify :: TypesSimplfy.SimplificationAlgorithm
  } deriving (Eq, Show)


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

