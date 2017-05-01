{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Endpoint.Dataset (
    get,
    put,
    train
) where

import Network.HTTP.Types
import Network.Wai
import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import Control.Exception (SomeException, try)
import Data.Text (unpack)
import Data.Aeson
import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.Map as Map
import Control.Arrow
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Maybe (fromMaybe)
import Data.Map (lookup)


import qualified Train
import DecisionTree

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

train setName requestBody = do
    maybeTrainVar <- ioBody
    case maybeTrainVar of
        Just trainVar -> trainInternal setName trainVar
        Nothing -> return $ responseBuilder status201 [("Content-Type", "text/html")] (fromString "missing targetvar from body")

    where   decodeMap = decode :: LazyBS.ByteString -> Maybe (Map.Map String String)
            ioBody = (>>= Map.lookup "targetvar") . decodeMap . LazyBS.fromStrict <$> requestBody

trainInternal setName trainVar = do
    fileContent <- try . readFile $ setFilePath
    fileContent' <- stringifyErr fileContent
    modelOrError <- return $ train' modelName trainVar fileContent'
    case modelOrError of
        Left err -> return $
            responseBuilder status400 [("Content-Type", "text/html")] (fromString err)
        Right model -> LazyBS.writeFile modelFilePath (encode model) >>
            (return $ responseLBS status201 [("Content-Type", "application/json")] (encode model))

    where   setFilePath     = "./datasets/" ++ setName ++ ".json"
            modelFilePath   = "./models/" ++ modelName ++ ".json"
            modelName       = setName ++ "_" ++ trainVar
            stringifyErr    = return . left show :: Either SomeException String -> IO (Either String String)
            getKey          = fromMaybe (Left "missing targetvar") . fmap Right . Data.Map.lookup "targetvar"

train' :: String -> String -> Either String String -> Either String Train.TrainingResult
train' modelName key fileContent = fileContent >>= eitherDecode . C8.pack >>= flip (Train.train modelName) key
