{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Fixtures where

import Data.Int (Int64)
import GHC.Generics (Generic)
import Rel8
import Rel8.PostGIS

-- HS Type
data TestTable f = TestTable
    { testId :: Column f Int64
    , testGeo :: Column f (Geo Point)
    }
    deriving (Generic, Rel8able)

-- Rel8 schema
testSchema :: TableSchema (TestTable Name)
testSchema =
    TableSchema
        { name = "rel8_postgis_test"
        , columns =
            TestTable
                { testId = "id"
                , testGeo = "geom"
                }
        }