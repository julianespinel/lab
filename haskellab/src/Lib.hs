{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Web.Scotty
import Network.HTTP.Types

someFunc :: IO ()
someFunc = scotty 3000 $ do
  get "/" $ do
    text "Hello world"
