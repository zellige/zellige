{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Geometry.VectorTile.Geometry where

import           Data.Foldable (foldl')
import qualified Data.Foldable as Foldable
import qualified Data.Sequence as S
import           GHC.Generics  (Generic)

data Unknown = Unknown deriving (Eq, Show, Generic)

data Point = Point {x :: !Int, y :: !Int} deriving (Eq, Show, Generic)

newtype LineString = LineString {lsPoints :: S.Seq Point} deriving (Eq, Show, Generic)

data Polygon = Polygon { polyPoints :: S.Seq Point
                       , inner      :: S.Seq Polygon } deriving (Eq, Show, Generic)

area :: Polygon -> Maybe Double
area p = fmap sum . sequence $ foldl' (\acc a -> area a : acc) [surveyor $ polyPoints p] (inner p)

-- | The surveyor's formula for calculating the area of a `Polygon`.
-- If the value reported here is negative, then the `Polygon` should be
-- considered an Interior Ring.
--
-- Assumption: The `V.Vector` given has at least 4 `Point`s.
surveyor :: S.Seq Point -> Maybe Double
surveyor (v'@((v'head S.:<| _) S.:|> v'last) S.:|> _) =
  case (v' S.|> v'head, v'last S.<| v') of
    (S.Empty, _)         -> Nothing
    (_ S.:<| _, S.Empty) -> Nothing
    (_ S.:<| tailYns, initYps S.:|> _) ->
      Just $ (/ 2) . fromIntegral . Foldable.foldl' (+) 0 $ S.zipWith3 (\xn yn yp -> xn * (yn - yp)) xs yns yps
      where
        xs = fmap x v'
        yns = fmap y tailYns
        yps = fmap y initYps
surveyor _ = Nothing
