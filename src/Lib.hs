{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Data.ByteString                 as BS
import           Data.Map.Lazy                   as DMZ
import           Data.Text                       (pack)
import           Data.Vector                     as DV
import           Data.Vector.Unboxed             as DVU
import           Geography.VectorTile
import           Geography.VectorTile.Geometry
import           Geography.VectorTile.VectorTile

someFunc :: IO ()
someFunc = putStrLn "someFunc"

writeOut = do
    _ <- BS.writeFile "/tmp/out.mvt" (encode $ untile t0)
    pure ()

t0 = VectorTile (DMZ.fromList [(pack "", l0)])
l0 = Layer 2 "water" DV.empty DV.empty (DV.fromList [f0]) 4096
f0 = Feature 0 props pv
props = DMZ.fromList [("uid", I64 123), ("foo", St "bar"), ("cat", St "flew")]
pv = DV.fromList [yyy]
yyy = Polygon xxx DV.empty
xxx = DVU.fromList ([(0, 0), (0,1), (1,1), (1,0), (0,0)] :: [(Int,Int)])

emptyLayer = Layer 2 "" DV.empty DV.empty DV.empty 4096
emptyProps = DMZ.empty
emptyPolyon = Polygon (DVU.empty :: DVU.Vector (Int, Int)) DV.empty

