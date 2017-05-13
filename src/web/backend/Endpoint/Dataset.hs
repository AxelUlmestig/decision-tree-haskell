{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Endpoint.Dataset (
    get,
    getAll,
    put,
    train
) where

import Control.Exception (catch)
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

import qualified Train
import DecisionTree

{- get data set functions -}

get :: Text -> IO Response
get setName = flip catch handler $ respond200 . LazyBS.pack <$> readFile filePath
    where   filePath    = "./datasets/" ++ unpack setName ++ ".json"
            handler     = (\_ -> return (respond404 "dataset not found")) :: IOError -> IO Response

{- get all datasets functions -}

getAll :: IO Response
getAll = listDirectory datasetsDir >>= sequence . map readDataset >>= return . respond200 . encode . extractDatasets
    where   datasetsDir = "./datasets/"
            readDataset = readFile . (datasetsDir++)

extractDatasets :: [String] -> [[Map String Value]]
extractDatasets = fromMaybe [] . sequence . filter isJust . map decode . map LazyBS.pack

{- put data set functions -}

put :: Text -> IO ByteString -> IO Response
put setName requestBody = requestBody >>= putBody setName

putBody :: Text -> ByteString -> IO Response
putBody setName body = fromMaybe errResponse $ putDataSet setName <$> (decodeDataSet body >>= Map.lookup "dataset")
    where   errResponse     = return $ respond400 "couldn't parse request body as dataset"
            decodeDataSet   = decode . LazyBS.fromStrict :: ByteString -> Maybe (Map String [Map String Value])

putDataSet :: Text -> [Map String Value] -> IO Response
putDataSet setName dataSet = LazyBS.writeFile filePath parsedDS >> return (respond200 parsedDS)
    where   filePath    = "./datasets/" ++ unpack setName ++ ".json"
            parsedDS    = encode dataSet

{- train data set functions -}

train :: Text -> IO ByteString -> IO Response
train setName requestBody = requestBody >>= trainBody setName

trainBody :: Text -> ByteString -> IO Response
trainBody setName body = fromMaybe errResponse $ trainKey setName <$> (decodeBody body >>= Map.lookup "targetvar")
    where   errResponse = return $ respond400 "missing 'targetvar' from body"
            decodeBody  = decode . LazyBS.fromStrict :: ByteString -> Maybe (Map String String)

trainKey :: Text -> String -> IO Response
trainKey setName targetvar = flip catch handler $ readFile filePath >>= trainRawDataSet trainF
    where   filePath    = "./datasets/" ++ unpack setName ++ ".json"
            handler     = (\_ -> return (respond404 "dataset not found")) :: IOError -> IO Response
            modelName   = unpack setName ++ "_" ++ targetvar
            trainF      = flip (Train.train modelName) targetvar

trainRawDataSet :: ([Map String Value] -> Either String Train.TrainingResult) -> String -> IO Response
trainRawDataSet trainF rawModel = fromMaybe errResponse $ trainModel trainF <$> decode (LazyBS.pack rawModel)
    where   errResponse = return $ respond500 "error parsing dataset"

trainModel :: ([Map String Value] -> Either String Train.TrainingResult) -> [Map String Value] -> IO Response
trainModel trainF dataset = either handleErr formatRes $ saveModel <$> trainF dataset
    where   handleErr   = return . respond400
            formatRes   = fmap (respond201 . encode)

saveModel :: Train.TrainingResult -> IO Train.TrainingResult
saveModel tr = LazyBS.writeFile filePath (encode tr) >> return tr
    where   filePath    = "./models/" ++ Train.name tr ++ ".json"

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
