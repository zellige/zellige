{-# LANGUAGE FlexibleContexts #-}

-- Liang Barsky Line Clipping Algorithm
-- https://en.wikipedia.org/wiki/Liang%E2%80%93Barsky_algorithm

module Data.Geometry.Clip.Internal.LiangBarskyLine (
 clipLines
) where

import qualified Data.Vector                   as Vector
import qualified Geography.VectorTile          as VectorTile

import qualified Data.Geometry.Types.Geography as TypesGeography

clipLines :: TypesGeography.BoundingBoxPts -> Vector.Vector VectorTile.LineString -> Vector.Vector VectorTile.LineString
clipLines _ _ = undefined

