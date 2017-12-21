{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Geometry.Types.MvtFeatures where

import qualified Data.Aeson                      as A
import qualified Data.HashMap.Strict             as HM
import qualified Data.Map.Lazy                   as DMZ
import qualified Data.Maybe                      as M
import           Data.Monoid
import qualified Data.Scientific                 as S
import qualified Data.Sequence                   as DS
import qualified Data.Text                       as T (Text)
import qualified Data.Vector                     as DV
import qualified Geography.VectorTile.Geometry   as VG
import qualified Geography.VectorTile.VectorTile as VT
import           Prelude                         hiding (Left, Right)

data MvtFeatures = MvtFeatures
  { mvtPoints   :: !(DS.Seq (VT.Feature VG.Point))
  , mvtLines    :: !(DS.Seq (VT.Feature VG.LineString))
  , mvtPolygons :: !(DS.Seq (VT.Feature VG.Polygon))
  } deriving (Show, Eq)

mkPoint :: Int -> A.Value -> DV.Vector VG.Point -> MvtFeatures
mkPoint x props p = MvtFeatures (mkFeature x props p) mempty mempty

mkLineString :: Int -> A.Value -> DV.Vector VG.LineString -> MvtFeatures
mkLineString x props l = MvtFeatures mempty (mkFeature x props l) mempty

mkPolygon :: Int -> A.Value -> DV.Vector VG.Polygon -> MvtFeatures
mkPolygon x props o = MvtFeatures mempty mempty (mkFeature x props o)

mkFeature :: Int -> A.Value -> DV.Vector g -> DS.Seq (VT.Feature g)
mkFeature x props geoms = DS.singleton $ VT.Feature x (convertProps props) geoms

convertProps :: A.Value -> DMZ.Map T.Text VT.Val
convertProps (A.Object x) = DMZ.fromList . M.catMaybes $ Prelude.fmap convertElems (HM.toList x)
convertProps _ = DMZ.empty

convertElems :: (t, A.Value) -> Maybe (t, VT.Val)
convertElems (k, A.String v) = Just (k, VT.St v)
convertElems (k, A.Number v) = Just (k, VT.Do (sToF v))
convertElems (k, A.Bool v)   = Just (k, VT.B v)
convertElems _               = Nothing

sToF :: S.Scientific -> Double
sToF = S.toRealFloat

instance Monoid MvtFeatures where
  mempty = MvtFeatures mempty mempty mempty
  mappend a b = MvtFeatures ((mvtPoints a) <> (mvtPoints b)) ((mvtLines a) <> (mvtLines b)) ((mvtPolygons a) <> (mvtPolygons b))
