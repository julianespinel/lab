{-# LANGUAGE OverloadedStrings #-}

module Messages.Message where

import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Data.Aeson
import Control.Applicative

-- Constructor
data Message = Message Text
  deriving (Show)

instance FromJSON Message where
  parseJSON (Object json) = Message <$> json .: "message"

instance ToJSON Message where
  toJSON (Message message) = object["message" .= message]
