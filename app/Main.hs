{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Options.Generic

import qualified Data.Geometry.MapnikVectorTile as MVT

main :: IO ()
main = do
  x <- unwrapRecord "Zellige - GeoJSON to MVT"
  MVT.writeLayer x



