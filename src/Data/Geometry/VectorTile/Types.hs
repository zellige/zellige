{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass        #-}

module Data.Geometry.VectorTile.Types where

import qualified Data.ByteString.Lazy              as BL
import           Data.Geometry.VectorTile.Geometry
import           Data.Hashable                     (Hashable)
import qualified Data.HashMap.Lazy                 as M
import           Data.Int
import qualified Data.Sequence                     as S
import           Data.Word
import           GHC.Generics                      (Generic)


newtype VectorTile = VectorTile {_layers :: M.HashMap BL.ByteString Layer} deriving (Eq, Show, Generic)

data Layer
  = Layer
      { -- | The version of the spec we follow. Should always be 2.
        _version     :: Word,
        _name        :: BL.ByteString,
        _unknowns    :: S.Seq (Feature (S.Seq Unknown)),
        _points      :: S.Seq (Feature (S.Seq Point)),
        _linestrings :: S.Seq (Feature (S.Seq LineString)),
        _polygons    :: S.Seq (Feature (S.Seq Polygon)),
        -- | Default: 4096
        _extent      :: Word
      }
  deriving (Eq, Show, Generic)

numberOfFeatures :: Layer -> Int
numberOfFeatures l = sum [S.length (_unknowns l), S.length (_points l), S.length (_linestrings l), S.length (_polygons l)]

data Feature gs
  = Feature
      { -- | Default: 0
        _featureId  :: Word,
        _metadata   :: M.HashMap BL.ByteString Val,
        _geometries :: gs
      }
  deriving (Eq, Show, Generic)

data Val = St BL.ByteString | Fl Float | Do Double | I64 Int64 | W64 Word64 | S64 Int64 | B Bool
  deriving (Eq, Ord, Show, Generic, Hashable)

