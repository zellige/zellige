module Main where

import           Criterion.Main

import qualified Data.Aeson                      as Aeson
import qualified Data.Aeson.Types                as AesonTypes
import qualified Data.Geospatial                 as Geospatial
import qualified Data.LinearRing                 as LinearRing
import qualified Data.LineString                 as LineString
import qualified Data.Sequence                   as Sequence
import qualified Data.Text                       as DataText
import qualified Data.Geometry.VectorTile.VectorTile            as VectorTile

import qualified Data.Geometry.Clip              as Clip
import qualified Data.Geometry.MapnikVectorTile  as MapnikVectorTile
import qualified Data.Geometry.Types.Config      as Config
import qualified Data.Geometry.Types.Config      as TypesConfig
import qualified Data.Geometry.Types.Geography   as TypesGeography
import qualified Data.Geometry.Types.LayerConfig as LayerConfig


main :: IO ()
main = do
  let tenLineStrings = simpleMultiLine (50 :: Double) (10 :: Double)
      oneHundredLineStrings = simpleMultiLine (50 :: Double) (100 :: Double)
      tenPoly = simplePoly (50 :: Double) (10 :: Double)
      oneHundredPoly = simplePoly (50 :: Double) (100 :: Double)
      oneThousandPoly = simplePoly (50 :: Double) (1000 :: Double)
      tenThousandPoly = simplePoly (50 :: Double) (10000 :: Double)
      multiTenPoly = Geospatial.mergeGeoPolygons $ Sequence.singleton tenPoly
      multiOneHundredPoly = Geospatial.mergeGeoPolygons $ Sequence.singleton oneHundredPoly
      multiOneThousandPoly = Geospatial.mergeGeoPolygons $ Sequence.singleton oneThousandPoly
      multiTenThousandPoly = Geospatial.mergeGeoPolygons $ Sequence.singleton tenThousandPoly
  defaultMain
    [ bgroup "writeFiles"
      [ bench "100 Points" $ nf (testPoly Clip.clipPolygonSh 100 boundBox tenPoly) Sequence.empty
      ]
    , bgroup "Clip LineString"
      [ bgroup "Quick Clip"
        [ bgroup "Size 10"
          [ bench "100 Points" $ nf (testLineString Clip.clipLinesQc 100 boundBox tenLineStrings) Sequence.empty
          , bench "1000 Points" $ nf (testLineString Clip.clipLinesQc 1000 boundBox tenLineStrings) Sequence.empty
          , bench "10000 Points" $ nf (testLineString Clip.clipLinesQc 10000 boundBox tenLineStrings) Sequence.empty
          ]
        , bgroup "Size 100"
          [ bench "100 Points" $ nf (testLineString Clip.clipLinesQc 100 boundBox oneHundredLineStrings) Sequence.empty
          , bench "1000 Points" $ nf (testLineString Clip.clipLinesQc 1000 boundBox oneHundredLineStrings) Sequence.empty
          , bench "10000 Points" $ nf (testLineString Clip.clipLinesQc 10000 boundBox oneHundredLineStrings) Sequence.empty
          ]
        ]
      , bgroup "Liang Barsky Clip"
        [ bgroup "Size 10"
          [ bench "100 Points" $ nf (testLineString Clip.clipLinesLb 100 boundBox tenLineStrings) Sequence.empty
          , bench "1000 Points" $ nf (testLineString Clip.clipLinesLb 1000 boundBox tenLineStrings) Sequence.empty
          , bench "10000 Points" $ nf (testLineString Clip.clipLinesLb 10000 boundBox tenLineStrings) Sequence.empty
          ]
        , bgroup "Size 100"
          [ bench "100 Points" $ nf (testLineString Clip.clipLinesLb 100 boundBox oneHundredLineStrings) Sequence.empty
          , bench "1000 Points" $ nf (testLineString Clip.clipLinesLb 1000 boundBox oneHundredLineStrings) Sequence.empty
          , bench "10000 Points" $ nf (testLineString Clip.clipLinesLb 10000 boundBox oneHundredLineStrings) Sequence.empty
          ]
        ]
      , bgroup "Nicholl-Lee-Nicholl Clip"
        [ bgroup "Size 10"
          [ bench "100 Points" $ nf (testLineString Clip.clipLinesNLN 100 boundBox tenLineStrings) Sequence.empty
          , bench "1000 Points" $ nf (testLineString Clip.clipLinesNLN 1000 boundBox tenLineStrings) Sequence.empty
          , bench "10000 Points" $ nf (testLineString Clip.clipLinesNLN 10000 boundBox tenLineStrings) Sequence.empty
          ]
        , bgroup "Size 100"
          [ bench "100 Points" $ nf (testLineString Clip.clipLinesNLN 100 boundBox oneHundredLineStrings) Sequence.empty
          , bench "1000 Points" $ nf (testLineString Clip.clipLinesNLN 1000 boundBox oneHundredLineStrings) Sequence.empty
          , bench "10000 Points" $ nf (testLineString Clip.clipLinesNLN 10000 boundBox oneHundredLineStrings) Sequence.empty
          ]
        ]
      , bgroup "Cohen Sutherland Clip"
        [ bgroup "Size 10"
          [ bench "100 Points" $ nf (testLineString Clip.clipLinesCs 100 boundBox tenLineStrings) Sequence.empty
          , bench "1000 Points" $ nf (testLineString Clip.clipLinesCs 1000 boundBox tenLineStrings) Sequence.empty
          , bench "10000 Points" $ nf (testLineString Clip.clipLinesCs 10000 boundBox tenLineStrings) Sequence.empty
          ]
        , bgroup "Size 100"
          [ bench "100 Points" $ nf (testLineString Clip.clipLinesCs 100 boundBox oneHundredLineStrings) Sequence.empty
          , bench "1000 Points" $ nf (testLineString Clip.clipLinesCs 1000 boundBox oneHundredLineStrings) Sequence.empty
          , bench "10000 Points" $ nf (testLineString Clip.clipLinesCs 10000 boundBox oneHundredLineStrings) Sequence.empty
          ]
        ]
      ]
    , bgroup "Clip Polygon"
      [ bgroup "Size 10"
        [ bench "100 Points" $ nf (testPoly Clip.clipPolygonQc 100 boundBox tenPoly) Sequence.empty
        , bench "1000 Points" $ nf (testPoly Clip.clipPolygonQc 1000 boundBox tenPoly) Sequence.empty
        , bench "10000 Points" $ nf (testPoly Clip.clipPolygonQc 10000 boundBox tenPoly) Sequence.empty
        ]
      , bgroup "Size 100"
        [ bench "10 Points" $ nf (testPoly Clip.clipPolygonQc 10 boundBox oneHundredPoly) Sequence.empty
        , bench "100 Points" $ nf (testPoly Clip.clipPolygonQc 100 boundBox oneHundredPoly) Sequence.empty
        , bench "1000 Points" $ nf (testPoly Clip.clipPolygonQc 1000 boundBox oneHundredPoly) Sequence.empty
        , bench "10000 Points" $ nf (testPoly Clip.clipPolygonQc 10000 boundBox oneHundredPoly) Sequence.empty
      ]
      , bgroup "Size 1000"
        [ bench "10 Points" $ nf (testPoly Clip.clipPolygonQc 10 boundBox oneThousandPoly) Sequence.empty
        , bench "100 Points" $ nf (testPoly Clip.clipPolygonQc 100 boundBox oneThousandPoly) Sequence.empty
        , bench "1000 Points" $ nf (testPoly Clip.clipPolygonQc 1000 boundBox oneThousandPoly) Sequence.empty
        , bench "10000 Points" $ nf (testPoly Clip.clipPolygonQc 10000 boundBox oneThousandPoly) Sequence.empty
        ]
      , bgroup "Size 10000"
        [ bench "10 Points" $ nf (testPoly Clip.clipPolygonQc 10 boundBox tenThousandPoly) Sequence.empty
        , bench "100 Points" $ nf (testPoly Clip.clipPolygonQc 100 boundBox tenThousandPoly) Sequence.empty
        , bench "1000 Points" $ nf (testPoly Clip.clipPolygonQc 1000 boundBox tenThousandPoly) Sequence.empty
        , bench "10000 Points" $ nf (testPoly Clip.clipPolygonQc 10000 boundBox tenThousandPoly) Sequence.empty
        ]
      ],
      bgroup "Clip MultiPolygon"
        [ bgroup "Size 10"
          [ bench "10 Points" $ nf (testPolys 10 boundBox multiTenPoly) Sequence.empty
          , bench "100 Points" $ nf (testPolys 100 boundBox multiTenPoly) Sequence.empty
          , bench "1000 Points" $ nf (testPolys 1000 boundBox multiTenPoly) Sequence.empty
          , bench "10000 Points" $ nf (testPolys 10000 boundBox multiTenPoly) Sequence.empty
          ]
        , bgroup "Size 100"
          [ bench "10 Points" $ nf (testPolys 10 boundBox multiOneHundredPoly) Sequence.empty
          , bench "100 Points" $ nf (testPolys 100 boundBox multiOneHundredPoly) Sequence.empty
          , bench "1000 Points" $ nf (testPolys 1000 boundBox multiOneHundredPoly) Sequence.empty
          , bench "10000 Points" $ nf (testPolys 10000 boundBox multiOneHundredPoly) Sequence.empty
        ]
        , bgroup "Size 1000"
          [ bench "10 Points" $ nf (testPolys 10 boundBox multiOneThousandPoly) Sequence.empty
          , bench "100 Points" $ nf (testPolys 100 boundBox multiOneThousandPoly) Sequence.empty
          , bench "1000 Points" $ nf (testPolys 1000 boundBox multiOneThousandPoly) Sequence.empty
          , bench "10000 Points" $ nf (testPolys 10000 boundBox multiOneThousandPoly) Sequence.empty
        ]
        , bgroup "Size 10000"
          [ bench "10 Points" $ nf (testPolys 10 boundBox multiTenThousandPoly) Sequence.empty
          , bench "100 Points" $ nf (testPolys 100 boundBox multiTenThousandPoly) Sequence.empty
          , bench "1000 Points" $ nf (testPolys 1000 boundBox multiTenThousandPoly) Sequence.empty
          , bench "10000 Points" $ nf (testPolys 10000 boundBox multiTenThousandPoly) Sequence.empty
        ]
      ]
    ]

simplePoly :: Double -> Double -> Geospatial.GeoPolygon
simplePoly radius total =
  Geospatial.GeoPolygon . Sequence.singleton $ simpleLinearRing radius total

simpleLinearRing :: Double -> Double -> LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS
simpleLinearRing radius total = LinearRing.makeLinearRing p1 p2 p3 rest
  where p1 = getCoord radius 0 total
        p2 = getCoord radius 1 total
        p3 = getCoord radius 2 total
        rest = Sequence.fromList $ map (\i -> getCoord radius i total) [3..total-1]

simpleMultiLine :: Double -> Double -> Geospatial.GeoMultiLine
simpleMultiLine radius total =
  Geospatial.GeoMultiLine . Sequence.singleton $ simpleLineString radius total

simpleLineString :: Double -> Double -> LineString.LineString Geospatial.GeoPositionWithoutCRS
simpleLineString radius total = LineString.makeLineString p1 p2 rest
  where p1 = getCoord radius 0 total
        p2 = getCoord radius 1 total
        rest = Sequence.fromList $ map (\i -> getCoord radius i total) [2..total-1]

testLineString :: (TypesGeography.BoundingBox
    -> Geospatial.GeoMultiLine
    -> Geospatial.GeoFeature Aeson.Value
    -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value)
    -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value))
  -> Integer
  -> TypesGeography.BoundingBox
  -> Geospatial.GeoMultiLine
  -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value)
  -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value)
testLineString _ 0 _ _ d    = d
testLineString algo counter bb line acc = testLineString algo (counter - 1) bb line (algo bb line geospatial acc)
  where geospatial = Geospatial.GeoFeature Nothing (Geospatial.MultiLine line) AesonTypes.Null Nothing

testPoly :: (TypesGeography.BoundingBox
    -> Geospatial.GeoPolygon
    -> Geospatial.GeoFeature Aeson.Value
    -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value)
    -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value))
  -> Integer
  -> TypesGeography.BoundingBox
  -> Geospatial.GeoPolygon
  -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value)
  -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value)
testPoly _ 0 _ _ acc    = acc
testPoly algo counter bb polygon acc = testPoly algo (counter - 1) bb polygon (algo bb polygon geospatial acc)
  where geospatial = Geospatial.GeoFeature Nothing (Geospatial.Polygon polygon) AesonTypes.Null Nothing

testPolys :: Integer -> TypesGeography.BoundingBox -> Geospatial.GeoMultiPolygon -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value) -> Sequence.Seq (Geospatial.GeoFeature Aeson.Value)
testPolys 0 _ _ acc = acc
testPolys counter bb multiPoly acc = testPolys (counter - 1) bb multiPoly (Clip.clipPolygonsSh bb multiPoly geospatial acc)
  where geospatial = Geospatial.GeoFeature Nothing (Geospatial.MultiPolygon multiPoly) AesonTypes.Null Nothing

getCoord :: Double -> Double -> Double -> Geospatial.GeoPositionWithoutCRS
getCoord radius current total = Geospatial.GeoPointXY $ Geospatial.PointXY x y
  where x = radius * sin ((360 / total) * current)
        y = radius * cos ((360 / total) * current)

boundBox :: TypesGeography.BoundingBox
boundBox = TypesGeography.bboxPtsToBbox $ TypesGeography.BoundingBoxPts (VectorTile.Point 0 0) (VectorTile.Point 1 1)

testConf :: Config.Config
testConf = Config.mkConfig (DataText.pack "demo") 15 (28999,19781) 128 2048 1 TypesConfig.NoAlgorithm

smallFC :: LayerConfig.LayerConfig
smallFC = LayerConfig.LayerConfig "./test/integration/small.json" "./dump/small.mvt" (DataText.pack "demo") 15 28999 19781 128 2048 1 TypesConfig.NoAlgorithm

testMain :: IO ()
testMain = MapnikVectorTile.writeLayer smallFC
