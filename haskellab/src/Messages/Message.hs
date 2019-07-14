{-# LANGUAGE OverloadedStrings #-}

module Messages.Message where

import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Data.Aeson
import Control.Applicative

import Database.PostgreSQL.Simple.FromRow

-- Constructor
data Message = Message { message :: Text }
  deriving (Show)

instance FromJSON Message where
  parseJSON (Object json) = Message <$> json .: "message"

instance ToJSON Message where
  toJSON (Message message) = object["message" .= message]

instance FromRow Message where
  fromRow = Message <$> field

