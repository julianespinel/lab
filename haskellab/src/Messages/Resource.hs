{-# LANGUAGE OverloadedStrings #-}

module Messages.Resource
    ( getAll
    , getFirst
    ) where

import Messages.Message
import Web.Scotty
import Network.HTTP.Types
import Control.Monad.IO.Class

import qualified Database.PostgreSQL.Simple as SQL
import qualified Messages.Persistence as P

getAll :: SQL.Connection -> ActionM ()
getAll psqlConn = do
  messages <- liftIO $ P.allMessages psqlConn
  json $ messages

getFirst :: SQL.Connection -> ActionM ()
getFirst psqlConn = do
  message <- liftIO $ P.firstMessage psqlConn
  json $ message
