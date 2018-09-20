module Main where

import           Criterion.Main

import qualified Data.Aeson                      as Aeson
import qualified Data.Aeson.Types                as AesonTypes
import qualified Data.Geospatial                 as Geospatial
import qualified Data.LinearRing                 as LinearRing
import qualified Data.LineString                 as LineString
import qualified Data.Text                       as DataText
import qualified Data.Vector                     as Vector
import qualified Data.Vector.Storable            as VectorStorable
import qualified Geography.VectorTile            as VectorTile

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
      multiTenPoly = Geospatial.mergeGeoPolygons $ Vector.singleton tenPoly
      multiOneHundredPoly = Geospatial.mergeGeoPolygons $ Vector.singleton oneHundredPoly
      multiOneThousandPoly = Geospatial.mergeGeoPolygons $ Vector.singleton oneThousandPoly
      multiTenThousandPoly = Geospatial.mergeGeoPolygons $ Vector.singleton tenThousandPoly
  defaultMain
    [ bgroup "writeFiles"
      [ bench "100 Points" $ nf (testPoly Clip.clipPolygon 100 boundBox tenPoly) Vector.empty
      ]
    , bgroup "Quick Bench Polygon"
      [ bench "Sutherland" $ nf (testPoly Clip.clipPolygon 10000 boundBox oneThousandPoly) Vector.empty
      , bench "Quick Clip" $ nf (testPoly Clip.clipPolygonQc 10000 boundBox oneThousandPoly) Vector.empty
      , bench "Nicholl-Lee-Nicholl" $ nf (testPoly Clip.clipPolygonNLN 10000 boundBox oneThousandPoly) Vector.empty
      ]
    , bgroup "Quick Bench LineString"
      [ bench "Cohen Sutherland" $ nf (testLineString Clip.clipLinesCs 10000 boundBox oneHundredLineStrings) Vector.empty
      , bench "Liang Barsky" $ nf (testLineString Clip.clipLinesLb 10000 boundBox oneHundredLineStrings) Vector.empty
      , bench "Quick Clip" $ nf (testLineString Clip.clipLinesQc 10000 boundBox oneHundredLineStrings) Vector.empty
      , bench "Nicholl-Lee-Nicholl" $ nf (testLineString Clip.clipLinesNLN 10000 boundBox oneHundredLineStrings) Vector.empty
      ]
    , bgroup "Clip LineString"
      [ bgroup "Quick Clip"
        [ bgroup "Size 10"
          [ bench "100 Points" $ nf (testLineString Clip.clipLinesQc 100 boundBox tenLineStrings) Vector.empty
          , bench "1000 Points" $ nf (testLineString Clip.clipLinesQc 1000 boundBox tenLineStrings) Vector.empty
          , bench "10000 Points" $ nf (testLineString Clip.clipLinesQc 10000 boundBox tenLineStrings) Vector.empty
          ]
        , bgroup "Size 100"
          [ bench "100 Points" $ nf (testLineString Clip.clipLinesQc 100 boundBox oneHundredLineStrings) Vector.empty
          , bench "1000 Points" $ nf (testLineString Clip.clipLinesQc 1000 boundBox oneHundredLineStrings) Vector.empty
          , bench "10000 Points" $ nf (testLineString Clip.clipLinesQc 10000 boundBox oneHundredLineStrings) Vector.empty
          ]
        ]
      , bgroup "Liang Barsky Clip"
        [ bgroup "Size 10"
          [ bench "100 Points" $ nf (testLineString Clip.clipLinesLb 100 boundBox tenLineStrings) Vector.empty
          , bench "1000 Points" $ nf (testLineString Clip.clipLinesLb 1000 boundBox tenLineStrings) Vector.empty
          , bench "10000 Points" $ nf (testLineString Clip.clipLinesLb 10000 boundBox tenLineStrings) Vector.empty
          ]
        , bgroup "Size 100"
          [ bench "100 Points" $ nf (testLineString Clip.clipLinesLb 100 boundBox oneHundredLineStrings) Vector.empty
          , bench "1000 Points" $ nf (testLineString Clip.clipLinesLb 1000 boundBox oneHundredLineStrings) Vector.empty
          , bench "10000 Points" $ nf (testLineString Clip.clipLinesLb 10000 boundBox oneHundredLineStrings) Vector.empty
          ]
        ]
      , bgroup "Nicholl-Lee-Nicholl Clip"
        [ bgroup "Size 10"
          [ bench "100 Points" $ nf (testLineString Clip.clipLinesNLN 100 boundBox tenLineStrings) Vector.empty
          , bench "1000 Points" $ nf (testLineString Clip.clipLinesNLN 1000 boundBox tenLineStrings) Vector.empty
          , bench "10000 Points" $ nf (testLineString Clip.clipLinesNLN 10000 boundBox tenLineStrings) Vector.empty
          ]
        , bgroup "Size 100"
          [ bench "100 Points" $ nf (testLineString Clip.clipLinesNLN 100 boundBox oneHundredLineStrings) Vector.empty
          , bench "1000 Points" $ nf (testLineString Clip.clipLinesNLN 1000 boundBox oneHundredLineStrings) Vector.empty
          , bench "10000 Points" $ nf (testLineString Clip.clipLinesNLN 10000 boundBox oneHundredLineStrings) Vector.empty
          ]
        ]
      , bgroup "Cohen Sutherland Clip"
        [ bgroup "Size 10"
          [ bench "100 Points" $ nf (testLineString Clip.clipLinesCs 100 boundBox tenLineStrings) Vector.empty
          , bench "1000 Points" $ nf (testLineString Clip.clipLinesCs 1000 boundBox tenLineStrings) Vector.empty
          , bench "10000 Points" $ nf (testLineString Clip.clipLinesCs 10000 boundBox tenLineStrings) Vector.empty
          ]
        , bgroup "Size 100"
          [ bench "100 Points" $ nf (testLineString Clip.clipLinesCs 100 boundBox oneHundredLineStrings) Vector.empty
          , bench "1000 Points" $ nf (testLineString Clip.clipLinesCs 1000 boundBox oneHundredLineStrings) Vector.empty
          , bench "10000 Points" $ nf (testLineString Clip.clipLinesCs 10000 boundBox oneHundredLineStrings) Vector.empty
          ]
        ]
      ]
    , bgroup "Clip Polygon"
      [ bgroup "Size 10"
        [ bench "100 Points" $ nf (testPoly Clip.clipPolygonQc 100 boundBox tenPoly) Vector.empty
        , bench "1000 Points" $ nf (testPoly Clip.clipPolygonQc 1000 boundBox tenPoly) Vector.empty
        , bench "10000 Points" $ nf (testPoly Clip.clipPolygonQc 10000 boundBox tenPoly) Vector.empty
        ]
      , bgroup "Size 100"
        [ bench "10 Points" $ nf (testPoly Clip.clipPolygonQc 10 boundBox oneHundredPoly) Vector.empty
        , bench "100 Points" $ nf (testPoly Clip.clipPolygonQc 100 boundBox oneHundredPoly) Vector.empty
        , bench "1000 Points" $ nf (testPoly Clip.clipPolygonQc 1000 boundBox oneHundredPoly) Vector.empty
        , bench "10000 Points" $ nf (testPoly Clip.clipPolygonQc 10000 boundBox oneHundredPoly) Vector.empty
      ]
      , bgroup "Size 1000"
        [ bench "10 Points" $ nf (testPoly Clip.clipPolygonQc 10 boundBox oneThousandPoly) Vector.empty
        , bench "100 Points" $ nf (testPoly Clip.clipPolygonQc 100 boundBox oneThousandPoly) Vector.empty
        , bench "1000 Points" $ nf (testPoly Clip.clipPolygonQc 1000 boundBox oneThousandPoly) Vector.empty
        , bench "10000 Points" $ nf (testPoly Clip.clipPolygonQc 10000 boundBox oneThousandPoly) Vector.empty
        ]
      , bgroup "Size 10000"
        [ bench "10 Points" $ nf (testPoly Clip.clipPolygonQc 10 boundBox tenThousandPoly) Vector.empty
        , bench "100 Points" $ nf (testPoly Clip.clipPolygonQc 100 boundBox tenThousandPoly) Vector.empty
        , bench "1000 Points" $ nf (testPoly Clip.clipPolygonQc 1000 boundBox tenThousandPoly) Vector.empty
        , bench "10000 Points" $ nf (testPoly Clip.clipPolygonQc 10000 boundBox tenThousandPoly) Vector.empty
        ]
      ],
      bgroup "Clip MultiPolygon"
        [ bgroup "Size 10"
          [ bench "10 Points" $ nf (testPolys 10 boundBox multiTenPoly) Vector.empty
          , bench "100 Points" $ nf (testPolys 100 boundBox multiTenPoly) Vector.empty
          , bench "1000 Points" $ nf (testPolys 1000 boundBox multiTenPoly) Vector.empty
          , bench "10000 Points" $ nf (testPolys 10000 boundBox multiTenPoly) Vector.empty
          ]
        , bgroup "Size 100"
          [ bench "10 Points" $ nf (testPolys 10 boundBox multiOneHundredPoly) Vector.empty
          , bench "100 Points" $ nf (testPolys 100 boundBox multiOneHundredPoly) Vector.empty
          , bench "1000 Points" $ nf (testPolys 1000 boundBox multiOneHundredPoly) Vector.empty
          , bench "10000 Points" $ nf (testPolys 10000 boundBox multiOneHundredPoly) Vector.empty
        ]
        , bgroup "Size 1000"
          [ bench "10 Points" $ nf (testPolys 10 boundBox multiOneThousandPoly) Vector.empty
          , bench "100 Points" $ nf (testPolys 100 boundBox multiOneThousandPoly) Vector.empty
          , bench "1000 Points" $ nf (testPolys 1000 boundBox multiOneThousandPoly) Vector.empty
          , bench "10000 Points" $ nf (testPolys 10000 boundBox multiOneThousandPoly) Vector.empty
        ]
        , bgroup "Size 10000"
          [ bench "10 Points" $ nf (testPolys 10 boundBox multiTenThousandPoly) Vector.empty
          , bench "100 Points" $ nf (testPolys 100 boundBox multiTenThousandPoly) Vector.empty
          , bench "1000 Points" $ nf (testPolys 1000 boundBox multiTenThousandPoly) Vector.empty
          , bench "10000 Points" $ nf (testPolys 10000 boundBox multiTenThousandPoly) Vector.empty
        ]
      ]
    ]

simplePoly :: Double -> Double -> Geospatial.GeoPolygon
simplePoly radius total =
  Geospatial.GeoPolygon . Vector.singleton $ simpleLinearRing radius total

simpleLinearRing :: Double -> Double -> LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS
simpleLinearRing radius total = LinearRing.makeLinearRing p1 p2 p3 rest
  where p1 = getCoord radius 0 total
        p2 = getCoord radius 1 total
        p3 = getCoord radius 2 total
        rest = VectorStorable.fromList $ map (\i -> getCoord radius i total) [3..total-1]

simpleMultiLine :: Double -> Double -> Geospatial.GeoMultiLine
simpleMultiLine radius total =
  Geospatial.GeoMultiLine . Vector.singleton $ simpleLineString radius total

simpleLineString :: Double -> Double -> LineString.LineString Geospatial.GeoPositionWithoutCRS
simpleLineString radius total = LineString.makeLineString p1 p2 rest
  where p1 = getCoord radius 0 total
        p2 = getCoord radius 1 total
        rest = VectorStorable.fromList $ map (\i -> getCoord radius i total) [2..total-1]

testLineString :: (TypesGeography.BoundingBox
    -> Geospatial.GeoMultiLine
    -> Geospatial.GeoFeature Aeson.Value
    -> Vector.Vector (Geospatial.GeoFeature Aeson.Value)
    -> Vector.Vector (Geospatial.GeoFeature Aeson.Value))
  -> Integer
  -> TypesGeography.BoundingBox
  -> Geospatial.GeoMultiLine
  -> Vector.Vector (Geospatial.GeoFeature Aeson.Value)
  -> Vector.Vector (Geospatial.GeoFeature Aeson.Value)
testLineString _ 0 _ _ d    = d
testLineString algo counter bb line acc = testLineString algo (counter - 1) bb line (algo bb line geospatial acc)
  where geospatial = Geospatial.GeoFeature Nothing (Geospatial.MultiLine line) AesonTypes.Null Nothing

testPoly :: (TypesGeography.BoundingBox
    -> Geospatial.GeoPolygon
    -> Geospatial.GeoFeature Aeson.Value
    -> Vector.Vector (Geospatial.GeoFeature Aeson.Value)
    -> Vector.Vector (Geospatial.GeoFeature Aeson.Value))
  -> Integer
  -> TypesGeography.BoundingBox
  -> Geospatial.GeoPolygon
  -> Vector.Vector (Geospatial.GeoFeature Aeson.Value)
  -> Vector.Vector (Geospatial.GeoFeature Aeson.Value)
testPoly _ 0 _ _ acc    = acc
testPoly algo counter bb polygon acc = testPoly algo (counter - 1) bb polygon (algo bb polygon geospatial acc)
  where geospatial = Geospatial.GeoFeature Nothing (Geospatial.Polygon polygon) AesonTypes.Null Nothing

testPolys :: Integer -> TypesGeography.BoundingBox -> Geospatial.GeoMultiPolygon -> Vector.Vector (Geospatial.GeoFeature Aeson.Value) -> Vector.Vector (Geospatial.GeoFeature Aeson.Value)
testPolys 0 _ _ acc = acc
testPolys counter bb multiPoly acc = testPolys (counter - 1) bb multiPoly (Clip.clipPolygons bb multiPoly geospatial acc)
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
