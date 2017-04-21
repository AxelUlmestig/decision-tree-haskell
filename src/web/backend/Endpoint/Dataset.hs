{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Endpoint.Dataset (
    get,
    put
) where

import Network.HTTP.Types
import Network.Wai
import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import Control.Exception (SomeException, try)
import Data.Text (unpack)
import Data.Aeson
import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.Map as Map

--get :: String -> IO Response
get setName = do
    fileContent <- try . readFile $ filePath
    case fileContent of
        Left (_ :: SomeException) -> return $
            responseBuilder status404 [("Content-Type", "text/html")] (fromString "dataset not found")
        Right content -> return $
            responseBuilder status200 [("Content-Type", "application/json")] (fromString content)

    where   filePath = "./datasets/" ++ (unpack setName) ++ ".json"

put setName requestBody = do
    body <- ioBody
    case body of
        Just dataset -> LazyBS.writeFile filePath dataset >> (return $
            responseBuilder status201 [] (fromString ""))
        Nothing -> return $
            responseBuilder status400 [("Content-Type", "text/html")] (fromString "dataset not found")

    where   filePath = "./datasets/" ++ (unpack setName) ++ ".json"
            decodeMap = decode :: LazyBS.ByteString -> Maybe (Map.Map String Value)
            ioBody = (fmap encode) . (>>= Map.lookup "dataset") . decodeMap . LazyBS.fromStrict <$> requestBody
