{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rel8.PostGIS.Instances where

import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Rel8

import Data.ByteString (ByteString)
import Rel8.PostGIS.Codec (decodeLineString, decodePoint, decodePolygon, encodeLineString, encodePoint, encodePolygon)
import Rel8.PostGIS.Types (Geo (..), LineString, Point, Polygon)

-- | Base Instance for Geometry Matching
instance DBType (Geo ByteString) where
    typeInformation = mapTypeInformation Geo unGeo geometryTypeInfo

geometryTypeInfo :: TypeInformation ByteString
geometryTypeInfo =
    let
        base = typeInformation @ByteString
     in
        base
            { typeName = "geometry"
            }

-- | Domain Instances for DBType
instance DBType (Geo Point) where
    typeInformation =
        mapTypeInformation
            (Geo . decodePoint . unGeo)
            (Geo . encodePoint . unGeo)
            (typeInformation @(Geo ByteString))

instance DBType (Geo LineString) where
    typeInformation =
        mapTypeInformation
            (Geo . decodeLineString . unGeo)
            (Geo . encodeLineString . unGeo)
            (typeInformation @(Geo ByteString))

instance DBType (Geo Polygon) where
    typeInformation =
        mapTypeInformation
            (Geo . decodePolygon . unGeo)
            (Geo . encodePolygon . unGeo)
            (typeInformation @(Geo ByteString))
