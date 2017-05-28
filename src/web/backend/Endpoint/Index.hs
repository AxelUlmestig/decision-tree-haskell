{-# LANGUAGE OverloadedStrings #-}

module Endpoint.Index (
    get
) where

import Network.HTTP.Types
import Network.Wai
import Blaze.ByteString.Builder.Char.Utf8 (fromString)

indexPath = "./src/web/frontend/build/index.html"
headers = [("Content-Type", "text/html")]

get :: IO Response
get = responseBuilder status200 headers . fromString <$> readFile indexPath
