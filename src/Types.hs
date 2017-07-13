module Types where

data BoundingBox = BoundingBox
  { _bbMinX :: Double
  , _bbMinY :: Double
  , _bbMaxX :: Double
  , _bbMaxY :: Double
  } deriving (Show, Eq)

data LatLon = LatLon
  { _llLat :: Double
  , _llLon :: Double }

data GoogleTileCoords = GoogleTileCoords
  { _gtcX    :: Integer
  , _gtcY    :: Integer
  , _gtcZoom :: Integer
  } deriving (Eq, Show)


