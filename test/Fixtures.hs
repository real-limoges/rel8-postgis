module Fixtures where

import Data.Int (Int64)
import Rel8
import Rel8.PostGIS
import Prelude

data TableFixture f = TableFixture
    { testId :: Column f Int64
    , testGeo :: Column f (Geo Point)
    }
    deriving (Show, Eq, Rel8able)

fixtureSchema :: TableSchema (TableFixture Name)
fixtureSchema =
    TableSchema
        { name = "rel8_postgis_test"
        , schema = Nothing
        , columns =
            TableFixture
                { testId = "id"
                , testGeo = "geom"
                }
        }