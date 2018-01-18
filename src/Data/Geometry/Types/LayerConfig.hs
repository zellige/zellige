{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Geometry.Types.LayerConfig where

import           Data.Semigroup            ((<>))
import qualified Data.Text                 as DT
import qualified Options.Applicative       as OA

import qualified Data.Geometry.Types.Types as DGT

data LayerConfig = LayerConfig
  { _layerInput          :: FilePath
  , _layerOutput         :: FilePath
  , _layerName           :: DT.Text
  , _layerZoom           :: DGT.Pixels
  , _layerX              :: DGT.Pixels
  , _layerY              :: DGT.Pixels
  , _layerBuffer         :: DGT.Pixels
  , _layerExtent         :: DGT.Pixels
  , _layerQuantizePixels :: DGT.Pixels
  } deriving (Show, Eq)

layerConfig :: OA.Parser LayerConfig
layerConfig = LayerConfig
  <$> OA.strOption
    ( OA.long "layer-input"
    <> OA.metavar "GEOJSON FILE"
    <> OA.help "Input GeoJSON file" )
  <*> OA.strOption
    ( OA.long "layer-output"
    <> OA.metavar "VECTOR FILE"
    <> OA.help "Output Mapnik Vector Tile file" )
  <*> OA.strOption
    ( OA.long "layer-name"
    <> OA.metavar "VECTOR LAYER NAME"
    <> OA.help "Name of Layer" )
  <*> OA.option OA.auto
    ( OA.long "layer-zoom"
    <> OA.help "Zoom level of layer"
    <> OA.metavar "INT" )
  <*> OA.option OA.auto
    ( OA.long "layer-x"
    <> OA.help "Longitude of layer"
    <> OA.metavar "INT" )
  <*> OA.option OA.auto
    ( OA.long "layer-y"
    <> OA.help "Latitude of layer"
    <> OA.metavar "INT" )
  <*> OA.option OA.auto
    ( OA.long "layer-buffer"
    <> OA.help "Buffer in pixels"
    <> OA.metavar "INT" )
  <*> OA.option OA.auto
    ( OA.long "layer-extent"
    <> OA.help "Layer size in pixels"
    <> OA.metavar "INT" )
  <*> OA.option OA.auto
    ( OA.long "layer-quantize-pixels"
    <> OA.help "Smallest pixel unit of layer"
    <> OA.metavar "INT" )
