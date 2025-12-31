{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# HLINT ignore "Use camelCase" #-}
-- \^ I'm not using camelCase in this module to match the PostGIS function names

module Rel8.PostGIS.Functions where

import Data.Int (Int32)
import Rel8 (Expr, DBType, function, lit)
import Rel8.PostGIS.Instances ()
import Rel8.PostGIS.Types

stX :: Expr (Geo Point) -> Expr (Maybe Double)
stX = function "ST_X"

stY :: Expr (Geo Point) -> Expr (Maybe Double)
stY = function "ST_Y"

st_makePoint :: Expr Double -> Expr Double -> Expr (Geo Point)
st_makePoint x y =
    function
        "ST_SetSRID"
        ( function "ST_MakePoint" (x, y) :: Expr (Geo Point)
        , lit 4326 :: Expr Int32
        )

st_distance :: (DBType (Geo a), DBType (Geo b)) => Expr (Geo a) -> Expr (Geo b) -> Expr Double
st_distance a b = function "ST_Distance" (a, b)

st_area :: Expr (Geo Polygon) -> Expr Double
st_area = function "ST_Area"

st_length :: Expr (Geo LineString) -> Expr Double
st_length = function "ST_Length"

st_dwithin :: (DBType (Geo a), DBType (Geo b)) => Expr (Geo a) -> Expr (Geo b) -> Expr Double -> Expr Bool
st_dwithin a b dist = function "ST_DWithin" (a, b, dist)

st_intersects :: (DBType (Geo a), DBType (Geo b)) => Expr (Geo a) -> Expr (Geo b) -> Expr Bool
st_intersects a b = function "ST_Intersects" (a, b)
