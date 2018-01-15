{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Geometry.Types.LayerConfig where

import qualified Data.Text                 as DT
import qualified Options.Generic           as OG
import           Numeric.Natural     (Natural)

import qualified Data.Geometry.Types.Types as DGT

data LayerConfig w = LayerConfig
  { _layerInput          :: w OG.::: FilePath OG.<?> "Input GeoJSON file"
  , _layerOutput         :: w OG.::: FilePath OG.<?> "Output Mapnik Vector Tile file"
  , _layerName           :: w OG.::: DT.Text OG.<?> "Name of layer"
  , _layerZoom           :: w OG.::: DGT.ZoomLevel OG.<?> "Zoom level of layer"
  , _layerX              :: w OG.::: Integer OG.<?> "Longitude of layer"
  , _layerY              :: w OG.::: Integer OG.<?> "Latitude of layer"
  , _layerBuffer         :: w OG.::: Natural OG.<?> "Buffer in pixels"
  , _layerExtent         :: w OG.::: Natural OG.<?> "Layer size in pixels"
  , _layerQuantizePixels :: w OG.::: Natural OG.<?> "Smallest pixel unit of layer"
  } deriving (OG.Generic)

modifiers :: OG.Modifiers
modifiers = OG.lispCaseModifiers

instance OG.ParseRecord (LayerConfig OG.Wrapped) where
   parseRecord = OG.parseRecordWithModifiers modifiers
