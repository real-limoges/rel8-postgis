module Rel8.PostGIS (
    Geo (..),
    Point,
    LineString,
    Polygon,
    Geometry,
    decodePoint,
    decodePolygon,
    encodePoint,
    encodePolygon,
)
where

import Rel8.PostGIS.Codec (decodePoint, decodePolygon, encodePoint, encodePolygon)
import Rel8.PostGIS.Functions
import Rel8.PostGIS.Instances ()
import Rel8.PostGIS.Types
