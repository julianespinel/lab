{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Messages.Resource
import Database.PostgreSQL.Simple
import qualified Data.Yaml.Config as YC

getPsqlConnection dbConfig = do
  host <- YC.lookup "host" dbConfig
  port <- YC.lookup "port" dbConfig
  username <- YC.lookup "username" dbConfig
  password <- YC.lookup "password" dbConfig
  database <- YC.lookup "database" dbConfig
  connect defaultConnectInfo
    { connectHost = host
    , connectPort = port
    , connectUser = username
    , connectPassword = password
    , connectDatabase = database
    }

main :: IO ()
main = do
  config <- YC.load "./dev.yml"
  dbConfig <- YC.subconfig "database" config
  psqlConn <- getPsqlConnection dbConfig

