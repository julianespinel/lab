{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Messages.Persistence where

import Messages.Message
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as SQL

allMessages :: SQL.Connection -> IO [Message]
allMessages psqlConn = SQL.query_ psqlConn "select * from messages;"


firstMessage :: SQL.Connection -> IO Message
firstMessage psqlConn = head <$> SQL.query_ psqlConn "select * from messages;"
