{-# LANGUAGE OverloadedStrings #-}

module TestInfrastructure where

import Control.Exception (bracket)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text.Encoding as TE
import qualified Hasql.Connection as C
import qualified Hasql.Connection.Setting as C.Setting
import qualified Hasql.Connection.Setting.Connection as C.Setting.Connection
import qualified Hasql.Session as S
import qualified Hasql.Statement as St
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D
import System.Environment (lookupEnv)
import Prelude

-- | Acquire -> Run -> Release
withTestDB :: (C.Connection -> IO a) -> IO a
withTestDB action = do
    let defaultConn = "postgres://postgres@localhost:5432/postgres"

    connStr <- maybe defaultConn BSC.pack <$> lookupEnv "DATABASE_URL"
    putStrLn $ "Connecting to: " ++ BSC.unpack connStr

    bracket (C.acquire [C.Setting.connection (C.Setting.Connection.string (TE.decodeUtf8 connStr))]) (either (const (pure ())) C.release) $ \res ->
        case res of
            Left err -> fail $ "Connection Failed: " ++ show err
            Right conn -> do
                setupTempTable conn
                action conn

setupTempTable :: C.Connection -> IO ()
setupTempTable conn = do
    let q = "CREATE TEMPORARY TABLE rel8_postgis_test (id int8, geom geometry(Point, 4326))"
    res <- S.run (rawSQL q) conn
    case res of
        Left err -> fail $ "Setup Failed: " ++ show err
        Right _ -> pure ()

rawSQL :: BSC.ByteString -> S.Session ()
rawSQL str = S.statement () $
    St.Statement str E.noParams D.noResult False