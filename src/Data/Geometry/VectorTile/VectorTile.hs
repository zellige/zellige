{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass #-}

module Data.Geometry.VectorTile.VectorTile
  ( -- * Vector Tiles
    VectorTile(..)
  , tile
  , untile
  , Layer(..)
  , Feature(..)
  , Val(..)
  -- * Geometries
  , Point(..)
  , LineString(..)
  , Polygon(..)
  , Protobuf
  , ProtobufGeom
  , Value
  , toProtobuf
  , unfeats
  , GeomVec
  ) where

import           Control.Monad ((>=>))
import           Data.Bifunctor (bimap)
import qualified Data.ByteString as BS
import           Data.ByteString.Lazy (fromStrict, toStrict)
import           Data.Text (Text, pack)
import           Data.Geometry.VectorTile.Internal (fromProtobuf, toProtobuf, unfeats, GeomVec, Protobuf, ProtobufGeom, Value)
import           Data.Geometry.VectorTile.Types
import           Data.Geometry.VectorTile.Geometry
import           Text.ProtocolBuffers.WireMessage (messageGet, messagePut)

-- | Attempt to parse a `VectorTile` from a strict collection of bytes.
tile :: BS.ByteString -> Either Text VectorTile
tile = bimap pack id . messageGet . fromStrict >=> fromProtobuf . fst

-- | Convert a `VectorTile` back into bytes.
untile :: VectorTile -> BS.ByteString
untile = toStrict . messagePut . toProtobuf
