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

{- get model functions -}

get :: Text -> IO Response
get = HF.get modelsDir

{- get all models functions -}

decodeModel :: LazyBS.ByteString -> Maybe TrainingResult
decodeModel = decode

getAll :: IO Response
getAll = HF.getAll decodeModel modelsDir

{- delete model functions -}

delete :: Text -> IO Response
delete = HF.delete modelsDir decodeModel modelFileName . matchModel

{- evaluate functions -}

evaluate :: Text -> IO ByteString -> IO Response
evaluate modelName requestBody = do
    body <- requestBody
    let eitherMap = parseBody body
    eitherModel <- getModel modelName
    let eitherDecisionTreeResult = askTree <$> eitherModel <*> eitherMap
    let eitherResponse = respond200 . encode <$> eitherDecisionTreeResult
    either return return eitherResponse

parseBody :: ByteString -> Either Response (Map String Value)
parseBody body = left badRequest . eitherDecode $ LazyBS.fromStrict body
    where badRequest = \_ -> respond400 "send json representation of data sample"

getModel :: Text -> IO (Either Response DecisionTree)
getModel modelName = do
    let filePath = modelsDir ++ unpack modelName ++ ".json"
    let handler = (\_ -> (return . Left $ respond404 "model not found")) :: IOError -> IO (Either Response LazyBS.ByteString)
    eitherRawFile <- handle handler $ Right <$> LazyBS.readFile filePath
    let eitherTrainingResult = eitherRawFile >>= left (\_ -> respond500 "error parsing model") . eitherDecode
    let eitherDecisionTree = model <$> eitherTrainingResult
    return eitherDecisionTree

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
