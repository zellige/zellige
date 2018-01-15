{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Geometry.Types.LayerConfig where

import qualified Data.Text                 as DT
import qualified Options.Generic           as OG

import qualified Data.Geometry.Types.Types as DGT

data LayerConfig w = LayerConfig
  { _layerInput          :: w OG.::: FilePath OG.<?> "Input GeoJSON file"
  , _layerOutput         :: w OG.::: FilePath OG.<?> "Output Mapnik Vector Tile file"
  , _layerName           :: w OG.::: DT.Text OG.<?> "Name of layer"
  , _layerZoom           :: w OG.::: DGT.ZoomLevel OG.<?> "Zoom level of layer"
  , _layerX              :: w OG.::: Integer OG.<?> "Longitude of layer"
  , _layerY              :: w OG.::: Integer OG.<?> "Latitude of layer"
  , _layerBuffer         :: w OG.::: Int OG.<?> "Buffer in pixels"
  , _layerExtent         :: w OG.::: Int OG.<?> "Layer size in pixels"
  , _layerQuantizePixels :: w OG.::: Int OG.<?> "Smallest pixel unit of layer"
  } deriving (OG.Generic)

modifiers :: OG.Modifiers
modifiers = OG.lispCaseModifiers

instance OG.ParseRecord (LayerConfig OG.Wrapped) where
   parseRecord = OG.parseRecordWithModifiers modifiers

configFromLayerConfig :: LayerConfig OG.Unwrapped -> DGT.Config
configFromLayerConfig lc = DGT.mkConfig (_layerName lc) (_layerZoom lc) (_layerX lc, _layerY lc) (DGT.Pixels $ _layerBuffer lc) (DGT.Pixels $ _layerExtent lc) (DGT.Pixels $ _layerQuantizePixels lc)
