{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import qualified Hasql.Session as Session
import qualified Hasql.Statement as Statement
import Hasql.Connection (Connection)
import Control.Monad.IO.Class (liftIO)
import Prelude

import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)

import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D

import TestInfrastructure (withTestDB)

main :: IO ()
main = withTestDB $ \conn -> do
    runTest "Round Trip Point" conn testRoundTripPoint

-- | Helper Runner
runTest :: String -> Connection -> Session.Session () -> IO ()
runTest name conn test = do
    res <- Session.run test conn
    case res of
        Left err -> error $ name ++ " Failed: " ++ show err
        Right _ -> putStrLn $ name ++ " Passed"

testRoundTripPoint :: Session.Session ()
testRoundTripPoint = do
    let targetX = 12.34
        targetY = 56.78
        testId = 1 :: Int64

    let insertSql = "INSERT INTO rel8_postgis_test (id, geom) VALUES ($1, ST_MakePoint($2, $3))"

        encoder :: E.Params (Int64, Double, Double)
        encoder =
               ((\(x,_,_) -> x) >$< E.param (E.nonNullable E.int8))
            <> ((\(_,y,_) -> y) >$< E.param (E.nonNullable E.float8))
            <> ((\(_,_,z) -> z) >$< E.param (E.nonNullable E.float8))

    Session.statement (testId, targetX, targetY) $ Statement.Statement insertSql encoder D.noResult True

    let selectSql = "SELECT ST_X(geom), ST_Y(geom) FROM rel8_postgis_test LIMIT 1"

        decoder = D.rowList $ (,)
            <$> D.column (D.nullable D.float8)
            <*> D.column (D.nullable D.float8)

    rows <- Session.statement () $ Statement.Statement selectSql E.noParams decoder True

    case rows of
        [] -> liftIO $ fail "No Rows Returned"
        (coords : _) -> validateCoords targetX targetY coords

validateCoords :: Double -> Double -> (Maybe Double, Maybe Double) -> Session.Session ()
validateCoords targetX targetY (actualX, actualY) = do
    let tolerance = 0.0001
        check val target = case val of
            Nothing -> False
            Just v  -> abs (v - target) < tolerance

    if check actualX targetX && check actualY targetY
        then pure ()
        else liftIO $ fail $ "Mismatch: " ++ show (actualX, actualY)