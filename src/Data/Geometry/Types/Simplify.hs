{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Geometry.Types.Simplify where

import qualified Data.Aeson                            as Aeson
import qualified Data.Char                             as Char
import qualified Data.String                           as String
import qualified Data.Vector.Storable                  as VectorStorable
import qualified Geography.VectorTile                  as VectorTile

import qualified Data.Geometry.Simplify.DouglasPeucker as SimplifyDouglasPeucker

data SimplificationAlgorithm = NoAlgorithm
  | Visvalingam
  | DouglasPeucker
  deriving (Eq, Show)

instance String.IsString SimplificationAlgorithm where
    fromString s =
        case Char.toLower <$> s of
            "visvalingam"     -> Visvalingam
            "douglas-peucker" -> DouglasPeucker
            _                 -> NoAlgorithm


simplifyUsing :: SimplificationAlgorithm -> VectorStorable.Vector VectorTile.Point -> VectorStorable.Vector VectorTile.Point
simplifyUsing NoAlgorithm    = id
simplifyUsing DouglasPeucker = SimplifyDouglasPeucker.douglasPeucker 1.0
simplifyUsing Visvalingam    = id

instance Aeson.ToJSON SimplificationAlgorithm where
  toJSON algo =
    Aeson.String $ case algo of
        NoAlgorithm    -> "none"
        Visvalingam    -> "visvalingam"
        DouglasPeucker -> "douglas-peucker"

instance Aeson.FromJSON SimplificationAlgorithm where
  parseJSON = Aeson.withText "SimplificationAlgorithm" $ \case
    "none"            -> pure NoAlgorithm
    "visvalingam"     -> pure Visvalingam
    "douglas-peucker" -> pure DouglasPeucker
    _                 -> fail "Unknown algorithm"

