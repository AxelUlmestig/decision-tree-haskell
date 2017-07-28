{-# LANGUAGE OverloadedStrings #-}

module Train (
    train,
    TrainingResult(..)
) where

import Data.Aeson
import Data.Map (delete, findWithDefault, Map)
import qualified Data.Map (lookup)
import Data.Monoid
import Data.List (group, maximumBy, minimumBy, sort)
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
    target      :: String,
    metaData    :: Map String DataType,
    model       :: DecisionTree
} deriving (Eq, Show)

instance ToJSON TrainingResult where
    toJSON tr = object [
        "name"      .= name tr,
        "target"    .= target tr,
        "model"     .= model tr,
        "metaData"  .= metaData tr
        ]

    toEncoding tr = pairs $
        "name"      .= name tr <>
        "target"    .= target tr <>
        "metaData"  .= metaData tr <>
        "model"     .= model tr

instance FromJSON TrainingResult where
    parseJSON (Object o)    = TrainingResult <$> o .: "name" <*> o .: "target" <*> o .: "metaData" <*> o .: "model"
    parseJSON _             = fail "can't parse object"

{- TrainingResult functions -}

train :: Double -> Dataset.Dataset -> String -> Either String TrainingResult
train significanceLevel dataset targetVariable = do
    let metaData = delete targetVariable $ Dataset.parameters dataset
    let modelName = Dataset.name dataset ++ "_" ++ targetVariable
    model <- trainModel significanceLevel (Dataset.content dataset) targetVariable
    return $ TrainingResult modelName targetVariable metaData model

{- DecisionTree construction functions -}

trainModel :: Double -> [Map String Value] -> String -> Either String DecisionTree
trainModel _ [] _                                           = Left "can't train based on empty data set"
trainModel significanceLevel trainingData targetVariable    = do
    let potentialFilters = getFilters $ map (delete targetVariable) trainingData
    chosenFilter <- bestFilter significanceLevel trainingData targetVariable potentialFilters
    constructTree significanceLevel trainingData targetVariable chosenFilter

constructTree :: Double -> [Map String Value] -> String -> Filter -> Either String DecisionTree
constructTree significanceLevel trainingData targetVariable fil =
    if informationGain trainingData (parseFilter fil) < entropyLimit
        then constructAnswer targetVariable trainingData
        else do
            let passedTData = filter (parseFilter fil) trainingData
            let failedTData = filter (not . parseFilter fil) trainingData
            affirmativeTree <- trainModel significanceLevel passedTData targetVariable
            negativeTree <- trainModel significanceLevel failedTData targetVariable
            return $ Question fil affirmativeTree negativeTree

constructAnswer :: String -> [Map String Value] -> Either String DecisionTree
constructAnswer _ []        = Left "can't construct Answer from empty data set"
constructAnswer targetVariable rawData = do
    pureData <- extractTrainingData targetVariable rawData
    let value = head . maximumBy (compare `on` length) . group . sort $ pureData
    let sampleSize = length pureData
    let confidence = (/ fromIntegral sampleSize) . fromIntegral . length . filter (==value) $ pureData
    return . Answer $ DecisionTreeResult value confidence sampleSize

{- filter evaluating functions -}

bestFilter :: Double -> [Map String Value] -> String -> [Filter] -> Either String Filter
bestFilter significanceLevel trainingData targetVariable potentialFilters = do
    let goodFilters = filterFilters significanceLevel trainingData targetVariable potentialFilters
    filterEntropyPairs <- mapM (getEntropy trainingData targetVariable) goodFilters
    let (_, bestFilter) = minimumBy (compare `on` fst) filterEntropyPairs
    return bestFilter

getEntropy :: [Map String Value] -> String -> Filter -> Either String (Float, Filter)
getEntropy values targetVariable f = do
    filteredValues <- extractTrainingData targetVariable $ filter (parseFilter f) values
    return (entropy filteredValues, f)

extractTrainingData :: String -> [Map String a] -> Either String [a]
extractTrainingData targetVariable = maybe missingKey Right . mapM (Data.Map.lookup targetVariable)
    where   missingKey  = Left $ "missing targetVariable in data: " ++ targetVariable

filterFilters :: Double -> [Map String Value] -> String -> [Filter] -> [Filter]
filterFilters significanceLevel trainingData targetVariable = (NullFilter:) . filter isStatisticallySignificant . filter hasAnyMatches
    where   hasAnyMatches               = not . null . flip filter trainingData . parseFilter
            isStatisticallySignificant  = statisticallySignificant significanceLevel trainingData targetVariable . parseFilter
