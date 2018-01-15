module Main where

import           Data.Semigroup                  ((<>))
import qualified Options.Applicative             as OA

import qualified Data.Geometry.MapnikVectorTile  as MVT
import qualified Data.Geometry.Types.LayerConfig as DGTL

main :: IO ()
main = do
  x <- OA.execParser opts
  MVT.writeLayer x
  where
    opts = OA.info (DGTL.layerConfig OA.<**> OA.helper) (OA.fullDesc <> OA.header "Zellige - GeoJSON to MVT")
