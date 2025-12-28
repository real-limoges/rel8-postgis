module Rel8.PostGIS.Types (
    Geo (..),
    Point (..),
    LineString (..),
    Polygon (..),
    Geometry (..),
) where

import Data.ByteString (ByteString)

-- Wrapping up haskell-postgis geometry for safety
-- (too many "Geometry" flying around)
newtype Geo a = Geo {unGeo :: a}
    deriving (Show, Eq)

data Point = Point Double Double
    deriving (Show, Eq)
newtype LineString = LineString [Point]
    deriving (Show, Eq)
newtype Polygon = Polygon [LineString]
    deriving (Show, Eq)

data Geometry
    = GeoPoint Point
    | GeoLineString LineString
    | GeoPolygon Polygon
    | GeoUnknown ByteString
    deriving (Show, Eq)
