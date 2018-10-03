{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Geometry.Types.Config where

import qualified Data.Aeson                    as Aeson
import qualified Data.ByteString.Lazy          as ByteStringLazy
import qualified Data.Char                     as Char
import qualified Data.Monoid                   as Monoid
import qualified Data.Semigroup                as Semigroup
import qualified Data.String                   as String
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as TextEncoding
import qualified Data.Word                     as Word

import qualified Data.Geometry.Types.Geography as TypesGeography

data Config = Config
  { _name           :: ByteStringLazy.ByteString
  , _gtc            :: TypesGeography.GoogleTileCoordsInt
  , _buffer         :: Word.Word
  , _extents        :: Int
  , _quantizePixels :: Int
  , _simplify       :: SimplificationAlgorithm
  , _version        :: Word.Word
  } deriving (Show, Eq)

mkConfig :: Text.Text -> TypesGeography.Pixels -> (TypesGeography.Pixels, TypesGeography.Pixels) -> TypesGeography.Pixels -> TypesGeography.Pixels -> TypesGeography.Pixels -> SimplificationAlgorithm -> Config
mkConfig name z (x, y) buffer extents quantizePixels simplify = Config ((ByteStringLazy.fromStrict . TextEncoding.encodeUtf8) name) (TypesGeography.mkGoogleTileCoordsInt z x y)
  (fromIntegral buffer) (fromIntegral extents) (TypesGeography.toInt quantizePixels) simplify TypesGeography.defaultVersion

-- Zoom Config

data ZoomConfig = ZoomConfig
  { _zcExtents  :: Int
  , _zcQuantize :: Int
  , _zcBBox     :: TypesGeography.BoundingBox
  , _zcSimplify :: SimplificationAlgorithm
  } deriving (Eq, Show)

-- Simplification

data SimplificationAlgorithm = NoAlgorithm
  | Visvalingam
  | DouglasPeucker
  deriving (Eq, Show)

instance String.IsString SimplificationAlgorithm where
  fromString s =
      case Char.toLower <$> s of
          "visvalingam"     -> Visvalingam
          "douglas-peucker" -> DouglasPeucker
          _                 -> NoAlgorithm

instance Aeson.ToJSON SimplificationAlgorithm where
  toJSON algo =
    Aeson.String $ case algo of
        NoAlgorithm    -> "none"
        Visvalingam    -> "visvalingam"
        DouglasPeucker -> "douglas-peucker"

instance Aeson.FromJSON SimplificationAlgorithm where
  parseJSON = Aeson.withText "SimplificationAlgorithm" $ \case
      "none"            -> pure NoAlgorithm
      "visvalingam"     -> pure Visvalingam
      "douglas-peucker" -> pure DouglasPeucker
      _                 -> fail "Unknown algorithm"

-- Options

data Options = Options
  { oVersion :: Monoid.Last Int
  , oName    :: Monoid.Last String
  , oExtent  :: Monoid.Last Int
  } deriving (Show, Eq)


instance Semigroup.Semigroup Options where
  (<>) x y = Options
    { oVersion = oVersion x Monoid.<> oVersion y
    , oName    = oName    x Monoid.<> oName    y
    , oExtent  = oExtent  x Monoid.<> oExtent  y
    }

instance Monoid Options where
  mempty = Options mempty mempty mempty

#if !(MIN_VERSION_base(4,11,0))
  -- this is redundant starting with base-4.11 / GHC 8.4
  -- if you want to avoid CPP, you can define `mappend = (<>)` unconditionally
  mappend = (<>)
#endif
