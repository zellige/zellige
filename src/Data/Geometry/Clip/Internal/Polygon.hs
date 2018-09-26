{-# LANGUAGE FlexibleContexts #-}

module Data.Geometry.Clip.Internal.Polygon (
  closeIfNot
) where

import qualified Data.Geospatial as Geospatial
import qualified Data.Sequence   as Sequence

closeIfNot :: Sequence.Seq Geospatial.PointXY -> Maybe (Sequence.Seq Geospatial.PointXY)
closeIfNot poly =
  case poly of
    (firstPt Sequence.:<| (_ Sequence.:|> lastPt)) ->
      if lastPt /= firstPt
        then Just $ lastPt Sequence.<| poly
        else Just poly
    _ -> Nothing
{-# INLINE closeIfNot #-}
