module Data.Geometry.SpecHelper where

import qualified Data.Vector.Storable as DataVectorStorable
import qualified Geography.VectorTile as VectorTile

tupleToPts :: [(Int, Int)] -> DataVectorStorable.Vector VectorTile.Point
tupleToPts = foldr (\(x,y) acc -> VectorTile.Point x y `DataVectorStorable.cons` acc) DataVectorStorable.empty
