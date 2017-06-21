{-# LANGUAGE OverloadedStrings #-}

module Endpoint.Model (
    delete,
    get,
    getAll,
    evaluate
) where

import Control.Exception (handle)
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LazyBS
import Data.Map (Map, singleton)
import Data.Maybe (fromMaybe, isJust)
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
evaluate modelName requestBody = requestBody >>= evaluateBody modelName

evaluateBody :: Text -> ByteString -> IO Response
evaluateBody modelName rawBody = fromMaybe badRequest $ evaluateModelPath modelName <$> decodeMap rawBody
    where   badRequest  = return $ respond400 "send json map representation of measurements"
            decodeMap   = decode . LazyBS.fromStrict

evaluateModelPath :: Text -> Map String Value -> IO Response
evaluateModelPath modelName obj = handle handler $ evaluateRawModel obj <$> readFile filePath
    where   handler     = (\_ -> return (respond404 "model not found")) :: IOError -> IO Response
            filePath    = modelsDir ++ unpack modelName ++ ".json"

evaluateRawModel :: Map String Value -> String -> Response
evaluateRawModel obj rawModel = fromMaybe errResponse $ evaluateModel obj <$> decodeDT rawModel
    where   errResponse = respond500 "error parsing model"
            decodeDT    = decode . LazyBS.pack

evaluateModel :: Map String Value -> TrainingResult -> Response
evaluateModel obj = respond200 . encode . flip askTree obj . model

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
