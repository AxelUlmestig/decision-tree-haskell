{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Endpoint.Dataset (
    delete,
    get,
    getAll,
    put,
    train
) where

import Control.Arrow (left)
import Control.Exception (handle)
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LazyBS
import qualified Data.Map as Map
import Data.Map (lookup, Map, singleton)
import Data.Text (Text, unpack)
import Network.HTTP.Types
import Network.Wai
import System.Directory

import qualified Endpoint.Internal.HandleFiles as HF

import qualified Train
import DecisionTree
import Dataset

significanceLevel = 0.05
datasetsDir = "./datasets/"
modelsDir = "./models/"

instance HF.Storable Dataset where
    match datasetName dataset = datasetName == name dataset
    fileName dataset = name dataset ++ ".json"

instance HF.Storable Train.TrainingResult where
    match trName trainingResult = trName == Train.name trainingResult
    fileName trainingResult = Train.name trainingResult ++ ".json"

{- get data set functions -}

get :: Text -> IO Response
get datasetTextName = do
    let datasetName = unpack datasetTextName
    eitherDataset <- HF.get datasetsDir datasetName :: IO (Either String Dataset)
    case eitherDataset of
        Left err -> return $ respond404 err
        Right dataset -> return . respond200 . encode $ dataset

{- get all datasets functions -}

getAll :: IO Response
getAll = do
    datasets <- HF.getAll datasetsDir :: IO [Dataset]
    return . respond200 . encode $ datasets

{- put data set functions -}

put :: Text -> IO ByteString -> IO Response
put setName requestBody = do
    body <- decode . LazyBS.fromStrict <$> requestBody :: IO (Maybe (Map String [Map String Value]))
    let maybeRawDataset = body >>= Map.lookup "dataset"
    let maybeDataset = prepareDataset (unpack setName) <$> maybeRawDataset :: Maybe Dataset
    case maybeDataset of
        Nothing -> return $ respond400 "couldn't parse request body as dataset"
        Just dataset -> do
            HF.save datasetsDir dataset
            return . respond200 $ encode dataset

{- delete data set functions -}

delete :: Text -> IO Response
delete datasetTextName = do
    let datasetName = unpack datasetTextName
    eitherDeleteResponse <- HF.delete datasetsDir datasetName :: IO (Either String (HF.DeleteResponse Dataset))
    case eitherDeleteResponse of
        Left err -> return . respond404 $ err
        Right deleteResponse -> return . respond200 . encode $ deleteResponse

{- train data set functions -}

train :: Text -> IO ByteString -> IO Response
train setTextName requestBody = do
    let setName = unpack setTextName
    body <- requestBody
    let eitherTargetVar = getTargetVar body
    eitherDataset <- left respond404 <$> HF.get datasetsDir setName
    let eitherTrainingResult = Train.train significanceLevel <$> eitherDataset <*> eitherTargetVar >>= left respond400
    case eitherTrainingResult of
        Left errorResponse -> return errorResponse
        Right trainingResult -> do
            HF.save modelsDir trainingResult
            return . respond200 . encode $ trainingResult

getTargetVar :: ByteString -> Either Response String
getTargetVar body = maybe err Right $ decodeBody body >>= Map.lookup "targetvar"
    where   err         = Left $ respond400 "missing 'targetvar' from body"
            decodeBody  = decode . LazyBS.fromStrict :: ByteString -> Maybe (Map String String)

{- misc functions -}

formatError :: String -> LazyBS.ByteString
formatError = encodeMap . singleton "err"
    where   encodeMap = encode :: Map String String -> LazyBS.ByteString

appJsonHeader :: ResponseHeaders
appJsonHeader = [("Content-Type", "application/json")]

respond200 = responseLBS status200 appJsonHeader
respond201 = responseLBS status201 appJsonHeader
respond400 = responseLBS status400 appJsonHeader . formatError
respond404 = responseLBS status404 appJsonHeader . formatError
respond500 = responseLBS status500 appJsonHeader . formatError
