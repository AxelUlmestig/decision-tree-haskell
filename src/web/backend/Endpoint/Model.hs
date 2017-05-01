{-# LANGUAGE OverloadedStrings #-}

module Endpoint.Model (
    get,
    getAll,
    evaluate
) where

import Control.Exception (catch)
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

{- get model functions -}

get :: Text -> IO Response
get modelName = flip catch handler $ respond200 . LazyBS.pack <$> readFile filePath
    where   filePath    = "./models/" ++ unpack modelName ++ ".json"
            handler     = (\_ -> return (respond404 "model not found")) :: IOError -> IO Response

{- get all models functions -}

getAll :: IO Response
getAll = listDirectory modelsDir >>= sequence . map readModel >>= return . respond200 . encode . extractModels
    where   modelsDir   = "./models/"
            readModel   = readFile . (modelsDir++)

extractModels :: [String] -> [TrainingResult]
extractModels = fromMaybe [] . sequence . filter isJust . map decode . map LazyBS.pack

{- evaluate functions -}

evaluate :: Text -> IO ByteString -> IO Response
evaluate modelName requestBody = requestBody >>= evaluateBody modelName

evaluateBody :: Text -> ByteString -> IO Response
evaluateBody modelName rawBody = fromMaybe badRequest $ evaluateModelPath modelName <$> decodeMap rawBody
    where   badRequest  = return $ respond400 "send json map representation of measurements"
            decodeMap   = decode . LazyBS.fromStrict

evaluateModelPath :: Text -> Map String Value -> IO Response
evaluateModelPath modelName obj = flip catch handler $ evaluateRawModel obj <$> readFile filePath
    where   handler     = (\_ -> return (respond404 "model not found")) :: IOError -> IO Response
            filePath    = "./models/" ++ unpack modelName ++ ".json"

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
