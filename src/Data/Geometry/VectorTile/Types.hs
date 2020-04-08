{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}

module Data.Geometry.VectorTile.Types (
  VectorTile(..)
  , Layer(..)
  , numberOfFeatures
  , Feature(..)
  , Val(..)
  , GeomVec
  , MvtFeatures(..)
  , emptyMvtFeatures
) where

import qualified Data.ByteString.Lazy              as BL
import qualified Data.Geometry.VectorTile.Geometry as G
import           Data.Hashable                     (Hashable)
import qualified Data.HashMap.Lazy                 as M
import           Data.Int
import qualified Data.Semigroup                    as Semigroup
import qualified Data.Sequence                     as Sequence
import qualified Data.Word                         as Word
import           GHC.Generics                      (Generic)


newtype VectorTile = VectorTile {_layers :: M.HashMap BL.ByteString Layer} deriving (Eq, Show, Generic)

data Layer
  = Layer
      { -- | The version of the spec we follow. Should always be 2.
        _version     :: Word,
        _name        :: BL.ByteString,
        _unknowns    :: Sequence.Seq (Feature (GeomVec G.Unknown)),
        _points      :: Sequence.Seq (Feature (GeomVec G.Point)),
        _linestrings :: Sequence.Seq (Feature (GeomVec G.LineString)),
        _polygons    :: Sequence.Seq (Feature (GeomVec G.Polygon)),
        -- | Default: 4096
        _extent      :: Word
      }
  deriving (Eq, Show, Generic)

numberOfFeatures :: Layer -> Int
numberOfFeatures l = sum [Sequence.length (_unknowns l), Sequence.length (_points l), Sequence.length (_linestrings l), Sequence.length (_polygons l)]

data Feature gs
  = Feature
      { -- | Default: 0
        _featureId  :: Maybe Word,
        _metadata   :: M.HashMap BL.ByteString Val,
        _geometries :: gs
      }
  deriving (Eq, Show, Generic)

data Val = St BL.ByteString | Fl Float | Do Double | I64 Int64 | W64 Word.Word64 | S64 Int64 | B Bool
  deriving (Eq, Ord, Show, Generic, Hashable)

type family GeomVec g = v | v -> g
type instance GeomVec G.Unknown      = Sequence.Seq G.Unknown
type instance GeomVec G.Point        = Sequence.Seq G.Point
type instance GeomVec G.LineString   = Sequence.Seq G.LineString
type instance GeomVec G.Polygon      = Sequence.Seq G.Polygon

data MvtFeatures = MvtFeatures
  { mvtUnknowns :: !(Sequence.Seq (Feature (GeomVec G.Unknown)))
  , mvtPoints   :: !(Sequence.Seq (Feature (GeomVec G.Point)))
  , mvtLines    :: !(Sequence.Seq (Feature (GeomVec G.LineString)))
  , mvtPolygons :: !(Sequence.Seq (Feature (GeomVec G.Polygon)))
  } deriving (Eq, Show)

instance Semigroup.Semigroup MvtFeatures where
  (<>) a b = MvtFeatures (mvtUnknowns a <> mvtUnknowns b) (mvtPoints a <> mvtPoints b) (mvtLines a <> mvtLines b) (mvtPolygons a <> mvtPolygons b)

instance Monoid MvtFeatures where
  mempty = MvtFeatures mempty mempty mempty mempty

#if !(MIN_VERSION_base(4,11,0))
  -- this is redundant starting with base-4.11 / GHC 8.4
  -- if you want to avoid CPP, you can define `mappend = (<>)` unconditionally
  mappend = (<>)
#endif

emptyMvtFeatures :: MvtFeatures
emptyMvtFeatures = MvtFeatures mempty mempty mempty mempty
