{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Endpoint.Dataset (
    delete,
    get,
    getAll,
    put,
    train
) where

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

datasetsDir = "./datasets/"

decodeDataset :: (LazyBS.ByteString -> Maybe Dataset)
decodeDataset = decode

matchDataset :: Text -> Dataset -> Bool
matchDataset setName = (== unpack setName) . name

datasetFileName :: Dataset -> FilePath
datasetFileName = (++".json") . name

{- get data set functions -}

get :: Text -> IO Response
get = HF.get datasetsDir

{- get all datasets functions -}

getAll :: IO Response
getAll = HF.getAll decodeDataset datasetsDir

{- put data set functions -}

put :: Text -> IO ByteString -> IO Response
put setName requestBody = requestBody >>= putBody setName

putBody :: Text -> ByteString -> IO Response
putBody setName body = fromMaybe errResponse $ putDataSet setName <$> (decodeDataSet body >>= Map.lookup "dataset")
    where   errResponse     = return $ respond400 "couldn't parse request body as dataset"
            decodeDataSet   = decode . LazyBS.fromStrict :: ByteString -> Maybe (Map String [Map String Value])

putDataSet :: Text -> [Map String Value] -> IO Response
putDataSet setName dataSet = LazyBS.writeFile filePath parsedDS >> return (respond200 parsedDS)
    where   setNameStr  = unpack setName
            filePath    = datasetsDir ++ setNameStr ++ ".json"
            parsedDS    = encode $ prepareDataset setNameStr dataSet

{- delete data set functions -}

delete :: Text -> IO Response
delete = HF.delete datasetsDir decodeDataset datasetFileName . matchDataset

{- train data set functions -}

train :: Text -> IO ByteString -> IO Response
train setName requestBody = requestBody >>= trainBody setName

trainBody :: Text -> ByteString -> IO Response
trainBody setName body = fromMaybe errResponse $ trainKey setName <$> (decodeBody body >>= Map.lookup "targetvar")
    where   errResponse = return $ respond400 "missing 'targetvar' from body"
            decodeBody  = decode . LazyBS.fromStrict :: ByteString -> Maybe (Map String String)

trainKey :: Text -> String -> IO Response
trainKey setName targetvar = handle handler $ readFile filePath >>= trainRawDataSet trainF
    where   filePath    = datasetsDir ++ unpack setName ++ ".json"
            handler     = (\_ -> return (respond404 "dataset not found")) :: IOError -> IO Response
            modelName   = unpack setName ++ "_" ++ targetvar
            trainF      = flip Train.train targetvar

trainRawDataSet :: (Dataset -> Either String Train.TrainingResult) -> String -> IO Response
trainRawDataSet trainF rawModel = fromMaybe errResponse $ trainModel trainF <$> decode (LazyBS.pack rawModel)
    where   errResponse = return $ respond500 "error parsing dataset"

trainModel :: (Dataset -> Either String Train.TrainingResult) -> Dataset -> IO Response
trainModel trainF dataset = either handleErr formatRes $ saveModel <$> trainF dataset
    where   handleErr   = return . respond400
            formatRes   = fmap (respond201 . encode)

saveModel :: Train.TrainingResult -> IO Train.TrainingResult
saveModel tr = LazyBS.writeFile filePath (encode tr) >> return tr
    where   filePath    = "./models/" ++ Train.name tr ++ ".json"

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
respond201 = responseLBS status201 appJsonHeader
respond400 = responseLBS status400 appJsonHeader . formatError
respond404 = responseLBS status404 appJsonHeader . formatError
respond500 = responseLBS status500 appJsonHeader . formatError
