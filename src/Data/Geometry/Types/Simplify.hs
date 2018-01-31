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

import           Data.Aeson as A

data SimplificationAlgorithm = NoAlgorithm
  | Visvalingam
  | DouglasPeucker
  deriving (Eq, Show)

instance ToJSON SimplificationAlgorithm where
  toJSON algo = String $ case algo of
    NoAlgorithm    -> "none"
    Visvalingam    -> "visvalingam"
    DouglasPeucker -> "douglas-peucker"

instance FromJSON SimplificationAlgorithm where
  parseJSON = withText "SimplificationAlgorithm" $ \t -> case t of
    "none"            -> pure NoAlgorithm
    "visvalingam"     -> pure Visvalingam
    "douglas-peucker" -> pure DouglasPeucker
    _                 -> fail "Unknown algorithm"

