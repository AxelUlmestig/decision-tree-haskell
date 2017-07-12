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
import Data.Maybe (fromMaybe, isJust)
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

decodeDataset :: (LazyBS.ByteString -> Maybe Dataset)
decodeDataset = decode

matchDataset :: Text -> Dataset -> Bool
matchDataset setName = (== unpack setName) . name

datasetFileName :: Dataset -> FilePath
datasetFileName = (++".json") . name

instance HF.Storable Dataset where
    match datasetName dataset = datasetName == name dataset
    fileName dataset = name dataset ++ ".json"

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

decodeBody :: ByteString -> Maybe (Map String [Map String Value])
decodeBody = decode . LazyBS.fromStrict

put :: Text -> IO ByteString -> IO Response
put setName requestBody = do
    let filePath = datasetsDir ++ unpack setName ++ ".json"
    body <- requestBody
    let maybeRawDataset = decodeBody body >>= Map.lookup "dataset"
    let maybeDataset = prepareDataset (unpack setName) <$> maybeRawDataset
    saveMaybeDataset filePath maybeDataset

saveMaybeDataset :: FilePath -> Maybe Dataset -> IO Response
saveMaybeDataset path (Just dataset) = do
    LazyBS.writeFile path (encode dataset)
    return . respond200 $ encode dataset
saveMaybeDataset _ Nothing = return $ respond400 "couldn't parse request body as dataset"

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
train setName requestBody = do
    body <- requestBody
    let eitherTargetVar = getTargetVar body
    eitherDataset <- getDataset setName
    let eitherTrainingResult = Train.train significanceLevel <$> eitherDataset <*> eitherTargetVar >>= left respond400
    saveTrainingResult eitherTrainingResult
    return $ either id (respond200 . encode) eitherTrainingResult

getTargetVar :: ByteString -> Either Response String
getTargetVar body = maybe err Right $ decodeBody body >>= Map.lookup "targetvar"
    where   err         = Left $ respond400 "missing 'targetvar' from body"
            decodeBody  = decode . LazyBS.fromStrict :: ByteString -> Maybe (Map String String)

getDataset :: Text -> IO (Either Response Dataset)
getDataset setName = do
    fileContent <- readFile $ datasetsDir ++ unpack setName ++ ".json"
    let maybeDataset = decode (LazyBS.pack fileContent)
    let err = Left $ respond500 "could not read dataset"
    let eitherDataset = maybe err Right maybeDataset
    let handler = (\_ -> (return . Left $ respond404 "dataset not found")) :: IOError -> IO (Either Response Dataset)
    handle handler (return eitherDataset)

saveTrainingResult :: Either Response Train.TrainingResult -> IO (Either Response Train.TrainingResult)
saveTrainingResult (Left response) = return (Left response)
saveTrainingResult (Right trainingResult) = do
    let filePath = "./models/" ++ Train.name trainingResult ++ ".json"
    LazyBS.writeFile filePath (encode trainingResult)
    return $ Right trainingResult

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
