module Main where

import qualified Data.Text                      as DT
import qualified Options.Generic                as OG

import qualified Data.Geometry.MapnikVectorTile as MVT

main :: IO ()
main = do
  x <- OG.unwrapRecord (DT.pack "Zellige - GeoJSON to MVT")
  MVT.writeLayer x
