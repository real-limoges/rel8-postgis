{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Fixtures


main :: IO ()
main = do
    connStr <- maybe "postgres://localhost:5432/postgres" id <$> lookupEnv "DATABASE_URL"
    putStrLn $ "Connecting to: " ++ connStr