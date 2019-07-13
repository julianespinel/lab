{-# LANGUAGE OverloadedStrings #-}

module Messages.Resource
    ( someFunc
    ) where

import Messages.Message
import Web.Scotty
import Network.HTTP.Types

someFunc :: IO ()
someFunc = scotty 3000 $ do
  get "/" $ do
    json $ Message "Hello world"
