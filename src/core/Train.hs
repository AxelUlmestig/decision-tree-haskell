{-# LANGUAGE OverloadedStrings #-}

module Train (
    train,
    TrainingResult(..)
) where

import Data.Aeson
import Data.Map (delete, findWithDefault, Map)
import qualified Data.Map (lookup)
import Data.Monoid
import Data.List (group, maximumBy, minimumBy)
import Data.Function (on)
import Control.Applicative

import qualified Dataset
import DecisionTree
import Filter
import GetFilters
import GetMetaData
import Entropy
import StatisticalSignificance

entropyLimit = 0.2

{- TrainingResult data type -}

data TrainingResult = TrainingResult {
    name        :: String,
    metaData    :: Map String DataType,
    model       :: DecisionTree
} deriving (Eq, Show)

instance ToJSON TrainingResult where
    toJSON tr = object [
        "name"      .= name tr,
        "model"     .= model tr,
        "metaData"  .= metaData tr
        ]

    toEncoding tr = pairs $
        "name"      .= name tr <>
        "metaData"  .= metaData tr <>
        "model"     .= model tr

instance FromJSON TrainingResult where
    parseJSON (Object o)    = TrainingResult <$> o .: "name" <*> o .: "metaData" <*> o .: "model"
    parseJSON _             = fail "can't parse object"

{- TrainingResult functions -}

train :: Double -> Dataset.Dataset -> String -> Either String TrainingResult
train significanceLevel dataset key = TrainingResult modelName metaData <$> trainModel significanceLevel (Dataset.content dataset) key
    where   metaData    = delete key $ Dataset.parameters dataset
            modelName   = Dataset.name dataset ++ "_" ++ key

{- DecisionTree construction functions -}

trainModel :: Double -> [Map String Value] -> String -> Either String DecisionTree
trainModel _ [] _                       = Left "can't train based on empty data set"
trainModel significanceLevel tData key  = fil >>= constructTree significanceLevel tData key
    where   fil     = bestFilter significanceLevel tData key . getFilters . map (delete key) $ tData

constructTree :: Double -> [Map String Value] -> String -> Filter -> Either String DecisionTree
constructTree significanceLevel tData key fil =
    if ig > entropyLimit
        then Question fil <$> posTree <*> negTree
        else constructAnswer key tData
    where   ig          = informationGain tData . parseFilter $ fil
            posTree     = trainModel significanceLevel passedTData key
            negTree     = trainModel significanceLevel failedTData key
            passedTData = filter (parseFilter fil) tData
            failedTData = filter (not . parseFilter fil) tData

constructAnswer :: String -> [Map String Value] -> Either String DecisionTree
constructAnswer _ [] = Left "can't construct Answer from empty data set"
constructAnswer key rawData = DecisionTreeResult <$> v <*> c <*> ss <**> return Answer
    where   v           = head . maximumBy (compare `on` length) . group <$> pureData
            c           = filter . (==) <$> v <*> pureData <**> return ((/) . fromIntegral . length) <*> fmap fromIntegral ss
            ss          = length <$> pureData
            pureData    = extractData key rawData

{- filter evaluating functions -}

bestFilter :: Double -> [Map String Value] -> String -> [Filter] -> Either String Filter
bestFilter significanceLevel tData key = fmap getLowestEntropy . applyGetEntropy . filterFilters significanceLevel tData key
    where   hasAnyMatches       = not . null . flip filter tData . parseFilter
            applyGetEntropy     = mapM (getEntropy tData key)
            getLowestEntropy    = snd . minimumBy compareEntropy
            compareEntropy      = compare `on` fst

getEntropy :: [Map String Value] -> String -> Filter -> Either String (Float, Filter)
getEntropy values key f = makePair <$> filteredEntropy f
    where   filteredEntropy = fmap entropy . extractData key . flip filter values . parseFilter
            makePair e      = (e, f)

extractData :: String -> [Map String a] -> Either String [a]
extractData key = maybe missingKey Right . mapM (Data.Map.lookup key)
    where   missingKey  = Left $ "missing key in data: " ++ key

filterFilters :: Double -> [Map String Value] -> String -> [Filter] -> [Filter]
filterFilters significanceLevel tData key = (NullFilter:) . filter isStatisticallySignificant . filter hasAnyMatches
    where   hasAnyMatches               = not . null . flip filter tData . parseFilter
            isStatisticallySignificant  = statisticallySignificant significanceLevel tData key . parseFilter
