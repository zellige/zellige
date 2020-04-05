{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module    : Geography.VectorTile.Util
-- Copyright : (c) Colin Woodbury 2016 - 2018
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>

module Data.Geometry.VectorTile.Util where

import           Data.Geometry.VectorTile.Geometry (Point (..))
import qualified Data.Sequence                     as Seq
import qualified Data.Text                         as Text

---

-- | A strict pair of Ints.
data Pair = Pair !Int !Int

-- | A sort of "self-zip", forming pairs from every two elements in a list.
-- Fails if there is an uneven number of elements.
safePairsWith :: (a -> Int) -> Seq.Seq a -> Either Text.Text (Seq.Seq Point)
safePairsWith f list = if null err then Right pts else Left "Uneven number of parameters given."
  where
    (pts, err) = go list
    go Seq.Empty = (Seq.empty, Seq.empty)
    go (a Seq.:<| Seq.Empty) = (Seq.empty, Seq.singleton a)
    go (a Seq.:<| b Seq.:<| rest) = (Point (f a) (f b) Seq.<| (fst . go $ rest), snd . go $ rest)

-- | Flatten a list of pairs. Equivalent to:
--
-- > ps ^.. each . both
unpairs :: [(a,a)] -> [a]
unpairs = foldr (\(a,b) acc -> a : b : acc) []
{-# INLINE unpairs #-}
