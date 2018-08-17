module Data.Geometry.SpecHelper where

import qualified Data.Vector.Storable as DataVectorStorable
import qualified Geography.VectorTile as VG

tupleToPts :: [(Int, Int)] -> DataVectorStorable.Vector VG.Point
tupleToPts = foldr (\(x,y) acc -> VG.Point x y `DataVectorStorable.cons` acc) DataVectorStorable.empty
