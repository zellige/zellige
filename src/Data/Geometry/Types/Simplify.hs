{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Geometry.Types.Simplify where

import qualified Data.Aeson                            as A
import qualified Data.Char                             as DC
import qualified Data.String                           as DS
import qualified Data.Vector.Unboxed                   as DVU
import qualified Geography.VectorTile                  as VG

import qualified Data.Geometry.Simplify.DouglasPeucker as DP

data SimplificationAlgorithm = NoAlgorithm
  | Visvalingam
  | DouglasPeucker
  deriving (Eq, Show)

instance DS.IsString SimplificationAlgorithm where
    fromString s =
        case (DC.toLower <$> s) of
            "visvalingam"     -> Visvalingam
            "douglas-peucker" -> DouglasPeucker
            _                 -> NoAlgorithm


simplifUsing :: SimplificationAlgorithm -> DVU.Vector VG.Point -> DVU.Vector VG.Point
simplifUsing NoAlgorithm    = id
simplifUsing DouglasPeucker = DP.douglasPeucker 1.0
simplifUsing Visvalingam    = id

instance A.ToJSON SimplificationAlgorithm where
  toJSON algo =
    A.String $ case algo of
        NoAlgorithm    -> "none"
        Visvalingam    -> "visvalingam"
        DouglasPeucker -> "douglas-peucker"

instance A.FromJSON SimplificationAlgorithm where
  parseJSON = A.withText "SimplificationAlgorithm" $ \t -> case t of
    "none"            -> pure NoAlgorithm
    "visvalingam"     -> pure Visvalingam
    "douglas-peucker" -> pure DouglasPeucker
    _                 -> fail "Unknown algorithm"

