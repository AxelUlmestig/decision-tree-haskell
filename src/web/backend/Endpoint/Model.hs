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

modelsDir = "./models/"

{- get model functions -}

get :: Text -> IO Response
get modelName = handle handler $ respond200 . LazyBS.pack <$> readFile filePath
    where   filePath    = modelsDir ++ unpack modelName ++ ".json"
            handler     = (\_ -> return (respond404 "model not found")) :: IOError -> IO Response

{- get all models functions -}

getAll :: IO Response
getAll = respond200 . encode <$> getAllInternal

getAllInternal :: IO [TrainingResult]
getAllInternal = extractModels <$> (listDirectory modelsDir >>= mapM readModel)
    where   readModel = readFile . (modelsDir++)

extractModels :: [String] -> [TrainingResult]
extractModels = fromMaybe [] . sequence . filter isJust . map (decode . LazyBS.pack)

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

{- delete model functions -}

data DeleteResponse = DeleteResponse {
    remaining   :: [TrainingResult],
    deleted     :: TrainingResult
}

instance ToJSON DeleteResponse where
    toJSON dr = object [
            "deleted"   .= deleted dr,
            "remaining" .= remaining dr
        ]

delete :: Text -> IO Response
delete modelName = formatDeleteResponse <$> (getAllInternal >>= deleteModel . deleteInternal modelName)

deleteInternal :: Text -> [TrainingResult] -> Either String DeleteResponse
deleteInternal modelName models = DeleteResponse remaining <$> deleted
    where   remaining   = filter ((/= unpack modelName) . name) models
            deleted     = eitherHead errMessage $ filter ((== unpack modelName) . name) models
            errMessage  = "dataset with the name " ++ unpack modelName ++ " was not found"

deleteModel :: Either String DeleteResponse -> IO (Either String DeleteResponse)
deleteModel (Left err)    = return (Left err)
deleteModel (Right dr)    = removeFile filePath >> return (Right dr)
    where   filePath    = modelsDir ++ name (deleted dr) ++ ".json"

formatDeleteResponse :: Either String DeleteResponse -> Response
formatDeleteResponse (Left err) = respond404 err
formatDeleteResponse (Right dr) = respond200 . encode $ dr

{- misc functions -}

eitherHead :: a -> [b] -> Either a b
eitherHead err [] = Left err
eitherHead _ (x:xs) = Right x

formatError :: String -> LazyBS.ByteString
formatError = encodeMap . singleton "err"
    where   encodeMap = encode :: Map String String -> LazyBS.ByteString

appJsonHeader :: ResponseHeaders
appJsonHeader = [("Content-Type", "application/json")]

respond200 = responseLBS status200 appJsonHeader
respond400 = responseLBS status400 appJsonHeader . formatError
respond404 = responseLBS status404 appJsonHeader . formatError
respond500 = responseLBS status500 appJsonHeader . formatError
