module Main where

import           Criterion.Main

import qualified Data.Geospatial                 as Geospatial
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
  let tenLineString = simpleLineString (50 :: Double) (10 :: Double)
      oneHundredLineString = simpleLineString (50 :: Double) (100 :: Double)
      tenLineStrings = Vector.fromList(generateArrayLineString 1 tenLineString [])
      oneHundredLineStrings = Vector.fromList(generateArrayLineString 1 oneHundredLineString [])
      tenPoly = simplePoly (50 :: Double) (10 :: Double)
      oneHundredPoly = simplePoly (50 :: Double) (100 :: Double)
      oneThousandPoly = simplePoly (50 :: Double) (1000 :: Double)
      tenThousandPoly = simplePoly (50 :: Double) (10000 :: Double)
      multiTenPoly = Vector.fromList(generateArrayPoly 1 tenPoly [])
      multiOneHundredPoly = Vector.fromList(generateArrayPoly 1 oneHundredPoly [])
      multiOneThousandPoly = Vector.fromList(generateArrayPoly 1 oneThousandPoly [])
      multiTenThousandPoly = Vector.fromList(generateArrayPoly 1 tenThousandPoly [])
  defaultMain
    [ bgroup "writeFiles"
      [ bench "100 Points" $ nf (testPoly Clip.clipPolygon 100 boundBox tenPoly) [Nothing]
      ]
    , bgroup "Quick Bench Poly"
      [ bench "Sutherland" $ nf (testPoly Clip.clipPolygon 10000 boundBox oneThousandPoly) []
      , bench "Quick Clip Lines" $ nf (testPoly Clip.clipPolygonQc 10000 boundBox oneThousandPoly) []
      , bench "NLN Lines" $ nf (testPoly Clip.clipPolygonNLN 10000 boundBox oneThousandPoly) []
      ]
    , bgroup "Quick Bench LineString"
      [ bench "Cohen Sutherland" $ nf (testLineString Clip.clipLinesCs 10000 boundBox oneHundredLineStrings) []
      , bench "Liang Barsky" $ nf (testLineString Clip.clipLinesLb 10000 boundBox oneHundredLineStrings) []
      , bench "Quick Clip" $ nf (testLineString Clip.clipLinesQc 10000 boundBox oneHundredLineStrings) []
      , bench "Nicholl-Lee-Nicholl" $ nf (testLineString Clip.clipLinesNLN 10000 boundBox oneHundredLineStrings) []
      ]
    , bgroup "Clip LineString"
      [ bgroup "Quick Clip"
        [ bgroup "Size 10"
          [ bench "100 Points" $ nf (testLineString Clip.clipLinesQc 100 boundBox tenLineStrings) []
          , bench "1000 Points" $ nf (testLineString Clip.clipLinesQc 1000 boundBox tenLineStrings) []
          , bench "10000 Points" $ nf (testLineString Clip.clipLinesQc 10000 boundBox tenLineStrings) []
          ]
        , bgroup "Size 100"
          [ bench "100 Points" $ nf (testLineString Clip.clipLinesQc 100 boundBox oneHundredLineStrings) []
          , bench "1000 Points" $ nf (testLineString Clip.clipLinesQc 1000 boundBox oneHundredLineStrings) []
          , bench "10000 Points" $ nf (testLineString Clip.clipLinesQc 10000 boundBox oneHundredLineStrings) []
          ]
        ]
      , bgroup "Liang Barsky Clip"
        [ bgroup "Size 10"
          [ bench "100 Points" $ nf (testLineString Clip.clipLinesLb 100 boundBox tenLineStrings) []
          , bench "1000 Points" $ nf (testLineString Clip.clipLinesLb 1000 boundBox tenLineStrings) []
          , bench "10000 Points" $ nf (testLineString Clip.clipLinesLb 10000 boundBox tenLineStrings) []
          ]
        , bgroup "Size 100"
          [ bench "100 Points" $ nf (testLineString Clip.clipLinesLb 100 boundBox oneHundredLineStrings) []
          , bench "1000 Points" $ nf (testLineString Clip.clipLinesLb 1000 boundBox oneHundredLineStrings) []
          , bench "10000 Points" $ nf (testLineString Clip.clipLinesLb 10000 boundBox oneHundredLineStrings) []
          ]
        ]
      , bgroup "Nicholl-Lee-Nicholl Clip"
        [ bgroup "Size 10"
          [ bench "100 Points" $ nf (testLineString Clip.clipLinesNLN 100 boundBox tenLineStrings) []
          , bench "1000 Points" $ nf (testLineString Clip.clipLinesNLN 1000 boundBox tenLineStrings) []
          , bench "10000 Points" $ nf (testLineString Clip.clipLinesNLN 10000 boundBox tenLineStrings) []
          ]
        , bgroup "Size 100"
          [ bench "100 Points" $ nf (testLineString Clip.clipLinesNLN 100 boundBox oneHundredLineStrings) []
          , bench "1000 Points" $ nf (testLineString Clip.clipLinesNLN 1000 boundBox oneHundredLineStrings) []
          , bench "10000 Points" $ nf (testLineString Clip.clipLinesNLN 10000 boundBox oneHundredLineStrings) []
          ]
        ]
      , bgroup "Cohen Sutherland Clip"
        [ bgroup "Size 10"
          [ bench "100 Points" $ nf (testLineString Clip.clipLinesCs 100 boundBox tenLineStrings) []
          , bench "1000 Points" $ nf (testLineString Clip.clipLinesCs 1000 boundBox tenLineStrings) []
          , bench "10000 Points" $ nf (testLineString Clip.clipLinesCs 10000 boundBox tenLineStrings) []
          ]
        , bgroup "Size 100"
          [ bench "100 Points" $ nf (testLineString Clip.clipLinesCs 100 boundBox oneHundredLineStrings) []
          , bench "1000 Points" $ nf (testLineString Clip.clipLinesCs 1000 boundBox oneHundredLineStrings) []
          , bench "10000 Points" $ nf (testLineString Clip.clipLinesCs 10000 boundBox oneHundredLineStrings) []
          ]
        ]
      ]
    , bgroup "Clip Polygon"
      [ bgroup "Size 10"
        [ bench "100 Points" $ nf (testPoly Clip.clipPolygonQc 100 boundBox tenPoly) [Nothing]
        , bench "1000 Points" $ nf (testPoly Clip.clipPolygonQc 1000 boundBox tenPoly) [Nothing]
        , bench "10000 Points" $ nf (testPoly Clip.clipPolygonQc 10000 boundBox tenPoly) [Nothing]
        ]
      , bgroup "Size 100"
        [ bench "10 Points" $ nf (testPoly Clip.clipPolygonQc 10 boundBox oneHundredPoly) [Nothing]
        , bench "100 Points" $ nf (testPoly Clip.clipPolygonQc 100 boundBox oneHundredPoly) [Nothing]
        , bench "1000 Points" $ nf (testPoly Clip.clipPolygonQc 1000 boundBox oneHundredPoly) [Nothing]
        , bench "10000 Points" $ nf (testPoly Clip.clipPolygonQc 10000 boundBox oneHundredPoly) [Nothing]
      ]
      , bgroup "Size 1000"
        [ bench "10 Points" $ nf (testPoly Clip.clipPolygonQc 10 boundBox oneThousandPoly) [Nothing]
        , bench "100 Points" $ nf (testPoly Clip.clipPolygonQc 100 boundBox oneThousandPoly) [Nothing]
        , bench "1000 Points" $ nf (testPoly Clip.clipPolygonQc 1000 boundBox oneThousandPoly) [Nothing]
        , bench "10000 Points" $ nf (testPoly Clip.clipPolygonQc 10000 boundBox oneThousandPoly) [Nothing]
        ]
      , bgroup "Size 10000"
        [ bench "10 Points" $ nf (testPoly Clip.clipPolygonQc 10 boundBox tenThousandPoly) [Nothing]
        , bench "100 Points" $ nf (testPoly Clip.clipPolygonQc 100 boundBox tenThousandPoly) [Nothing]
        , bench "1000 Points" $ nf (testPoly Clip.clipPolygonQc 1000 boundBox tenThousandPoly) [Nothing]
        , bench "10000 Points" $ nf (testPoly Clip.clipPolygonQc 10000 boundBox tenThousandPoly) [Nothing]
        ]
      ],
      bgroup "Clip MultiPolygon "
        [ bgroup "Size 10"
          [ bench "10 Points" $ nf (testPolys 10 boundBox multiTenPoly) []
          , bench "100 Points" $ nf (testPolys 100 boundBox multiTenPoly) []
          , bench "1000 Points" $ nf (testPolys 1000 boundBox multiTenPoly) []
          , bench "10000 Points" $ nf (testPolys 10000 boundBox multiTenPoly) []
          ]
        , bgroup "Size 100"
          [ bench "10 Points" $ nf (testPolys 10 boundBox multiOneHundredPoly) []
          , bench "100 Points" $ nf (testPolys 100 boundBox multiOneHundredPoly) []
          , bench "1000 Points" $ nf (testPolys 1000 boundBox multiOneHundredPoly) []
          , bench "10000 Points" $ nf (testPolys 10000 boundBox multiOneHundredPoly) []
        ]
        , bgroup "Size 1000"
          [ bench "10 Points" $ nf (testPolys 10 boundBox multiOneThousandPoly) []
          , bench "100 Points" $ nf (testPolys 100 boundBox multiOneThousandPoly) []
          , bench "1000 Points" $ nf (testPolys 1000 boundBox multiOneThousandPoly) []
          , bench "10000 Points" $ nf (testPolys 10000 boundBox multiOneThousandPoly) []
        ]
        , bgroup "Size 10000"
          [ bench "10 Points" $ nf (testPolys 10 boundBox multiTenThousandPoly) []
          , bench "100 Points" $ nf (testPolys 100 boundBox multiTenThousandPoly) []
          , bench "1000 Points" $ nf (testPolys 1000 boundBox multiTenThousandPoly) []
          , bench "10000 Points" $ nf (testPolys 10000 boundBox multiTenThousandPoly) []
        ]
      ]
    ]

simplePoly :: (Floating a, RealFrac a) => a -> a -> VectorTile.Polygon
simplePoly radius total = VectorTile.Polygon (VectorStorable.fromList (getPoints radius total)) (Vector.fromList [])

simpleLineString :: (Floating a, RealFrac a) => a -> a -> VectorTile.LineString
simpleLineString radius total = VectorTile.LineString (VectorStorable.fromList (getPoints radius total))

testPoly :: (TypesGeography.BoundingBoxPts -> VectorTile.Polygon -> Maybe VectorTile.Polygon) -> Integer -> TypesGeography.BoundingBoxPts -> VectorTile.Polygon -> [Maybe VectorTile.Polygon] -> [Maybe VectorTile.Polygon]
testPoly _ 0 _ _ d    = d
testPoly algo a b c d = d ++ testPoly algo (a - 1) b c [algo b c]

testLineString :: (TypesGeography.BoundingBoxPts -> Vector.Vector VectorTile.LineString -> Vector.Vector VectorTile.LineString) -> Integer -> TypesGeography.BoundingBoxPts -> Vector.Vector VectorTile.LineString -> [Vector.Vector VectorTile.LineString] -> [Vector.Vector VectorTile.LineString]
testLineString _ 0 _ _ d    = d
testLineString algo a b c d = d ++ testLineString algo (a - 1) b c [algo b c]

testPolys :: Integer -> TypesGeography.BoundingBoxPts -> Vector.Vector VectorTile.Polygon -> [Vector.Vector VectorTile.Polygon] -> [Vector.Vector VectorTile.Polygon]
testPolys 0 _ _ d = d
testPolys a b c d = d ++ testPolys (a - 1) b c [Clip.clipPolygons b c]

generateArrayPoly :: Integer -> VectorTile.Polygon -> [VectorTile.Polygon] -> [VectorTile.Polygon]
generateArrayPoly 0 _ c = c
generateArrayPoly a b c = c ++ generateArrayPoly (a - 1) b [b]

generateArrayLineString :: Integer -> VectorTile.LineString -> [VectorTile.LineString] -> [VectorTile.LineString]
generateArrayLineString 0 _ c = c
generateArrayLineString a b c = c ++ generateArrayLineString (a - 1) b [b]

getPoints :: Double -> Double -> [Geospatial.PointXY]
getPoints radius total = getPoints' radius total total []

getPoints' :: Double -> Double -> Double -> [Geospatial.PointXY] -> [Geospatial.PointXY]
getPoints' _ 0 _ b = b
getPoints' radius current total b = b ++ getPoints' radius (current - 1) total [getCoord radius current total]

getCoord :: Double -> Double -> Double -> Geospatial.PointXY
getCoord radius current total = Geospatial.PointXY (getX radius current total)  (getY radius current total)

getX :: Double -> Double -> Double -> Double
getX radius current total = radius * sin ((360 / total) * current)

getY :: Double -> Double -> Double -> Double
getY radius current total = radius * cos ((360 / total) * current)

boundBox :: TypesGeography.BoundingBox
boundBox = TypesGeography.bboxPtsToBbox $ TypesGeography.BoundingBoxPts (VectorTile.Point 0 0) (VectorTile.Point 1 1)

testConf :: Config.Config
testConf = Config.mkConfig (DataText.pack "demo") 15 (28999,19781) 128 2048 1 TypesConfig.NoAlgorithm

smallFC :: LayerConfig.LayerConfig
smallFC = LayerConfig.LayerConfig "./test/integration/small.json" "./dump/small.mvt" (DataText.pack "demo") 15 28999 19781 128 2048 1 TypesConfig.NoAlgorithm

testMain :: IO ()
testMain = MapnikVectorTile.writeLayer smallFC
