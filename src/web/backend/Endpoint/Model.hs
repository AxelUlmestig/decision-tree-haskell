{-# LANGUAGE OverloadedStrings #-}

module Endpoint.Model (
    delete,
    get,
    getAll,
    evaluate
) where

import Control.Arrow (left)
import Control.Exception (handle)
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LazyBS
import Data.Map (Map, singleton)
import Data.Text (Text, unpack)
import Network.HTTP.Types
import Network.Wai
import System.Directory

import DecisionTree
import Train
import qualified Endpoint.Internal.HandleFiles as HF

modelsDir = "./models/"

modelFileName :: TrainingResult -> FilePath
modelFileName = (++".json") . name

matchModel :: Text -> TrainingResult -> Bool
matchModel modelName = (== unpack modelName) . name

instance HF.Storable TrainingResult where
    match trName trainingResult = trName == name trainingResult
    fileName trainingResult = name trainingResult ++ ".json"

{- get model functions -}

get :: Text -> IO Response
get trainingResultTextName = do
    let trainingResultName = unpack trainingResultTextName
    eitherTrainingResult <- HF.get modelsDir trainingResultName :: IO (Either String TrainingResult)
    case eitherTrainingResult of
        Left err -> return $ respond404 err
        Right trainingResult -> return . respond200 . encode $ trainingResult

{- get all models functions -}

getAll :: IO Response
getAll = do
    trainingResults <- HF.getAll modelsDir :: IO [TrainingResult]
    return . respond200 . encode $ trainingResults

{- delete model functions -}

delete :: Text -> IO Response
delete trainingResultTextName = do
    let trainingResultName = unpack trainingResultTextName
    eitherDeleteResponse <- HF.delete modelsDir trainingResultName :: IO (Either String (HF.DeleteResponse TrainingResult))
    case eitherDeleteResponse of
        Left err -> return . respond404 $ err
        Right deleteResponse -> return . respond200 . encode $ deleteResponse

{- evaluate functions -}

evaluate :: Text -> IO ByteString -> IO Response
evaluate modelTextName requestBody = do
    let modelName = unpack modelTextName
    body <- requestBody
    let eitherMap = parseBody body
    eitherTrainingResult <- left respond404 <$> HF.get modelsDir modelName :: IO (Either Response TrainingResult)
    let eitherDecisionTreeResult = askTree . model <$> eitherTrainingResult <*> eitherMap
    let eitherResponse = respond200 . encode <$> eitherDecisionTreeResult
    either return return eitherResponse

parseBody :: ByteString -> Either Response (Map String Value)
parseBody body = left badRequest . eitherDecode $ LazyBS.fromStrict body
    where badRequest = \_ -> respond400 "send json representation of data sample"

{- misc functions -}

formatError :: String -> LazyBS.ByteString
formatError = encodeMap . singleton "err"
    where   encodeMap = encode :: Map String String -> LazyBS.ByteString

appJsonHeader :: ResponseHeaders
appJsonHeader = [("Content-Type", "application/json")]

respond200 = responseLBS status200 appJsonHeader
respond400 = responseLBS status400 appJsonHeader . formatError
respond404 = responseLBS status404 appJsonHeader . formatError
respond500 = responseLBS status500 appJsonHeader . formatError
