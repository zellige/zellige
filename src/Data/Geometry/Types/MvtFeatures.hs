{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Geometry.Types.MvtFeatures where

import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict  as HM
import qualified Data.Maybe           as M
import           Data.Monoid
import qualified Data.Scientific      as S
import qualified Data.Sequence        as DS
import qualified Data.Text            as T (Text)
import qualified Data.Text.Encoding   as DTE
import qualified Geography.VectorTile as VG
import           Prelude              hiding (Left, Right)

data MvtFeatures = MvtFeatures
  { mvtPoints   :: !(DS.Seq (VG.Feature VG.Point))
  , mvtLines    :: !(DS.Seq (VG.Feature VG.LineString))
  , mvtPolygons :: !(DS.Seq (VG.Feature VG.Polygon))
  } deriving (Show, Eq)

mkPoint :: Word -> A.Value -> DS.Seq VG.Point -> MvtFeatures
mkPoint x props p = MvtFeatures (mkFeature x props p) mempty mempty

mkLineString :: Word -> A.Value -> DS.Seq VG.LineString -> MvtFeatures
mkLineString x props l = MvtFeatures mempty (mkFeature x props l) mempty

mkPolygon :: Word -> A.Value -> DS.Seq VG.Polygon -> MvtFeatures
mkPolygon x props o = MvtFeatures mempty mempty (mkFeature x props o)

mkFeature :: Word -> A.Value -> DS.Seq g -> DS.Seq (VG.Feature g)
mkFeature x props geoms = DS.singleton $ VG.Feature x (convertProps props) geoms

convertProps :: A.Value -> HM.HashMap LBS.ByteString VG.Val
convertProps (A.Object x) = HM.fromList . M.catMaybes $ Prelude.fmap convertElems (HM.toList x)
convertProps _ = HM.empty

convertElems :: (T.Text, A.Value) -> Maybe (LBS.ByteString, VG.Val)
convertElems (k, A.String v) = Just ((LBS.fromStrict . DTE.encodeUtf8) k, VG.St ((LBS.fromStrict . DTE.encodeUtf8) v))
convertElems (k, A.Number v) = Just ((LBS.fromStrict . DTE.encodeUtf8) k, VG.Do (sToF v))
convertElems (k, A.Bool v)   = Just ((LBS.fromStrict . DTE.encodeUtf8) k, VG.B v)
convertElems _               = Nothing

sToF :: S.Scientific -> Double
sToF = S.toRealFloat

instance Monoid MvtFeatures where
  mempty = MvtFeatures mempty mempty mempty
  mappend a b = MvtFeatures (mvtPoints a <> mvtPoints b) (mvtLines a <> mvtLines b) (mvtPolygons a <> mvtPolygons b)
