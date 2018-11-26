{-# LANGUAGE OverloadedStrings #-}

module Train (
    train,
    TrainingParameters(..),
    TrainingResult(..)
) where

import Control.Parallel             (par, pseq)
import Control.Parallel.Strategies  (parListChunk, rdeepseq, using)
import Data.Aeson                   ((.:), (.=), FromJSON, fromJSON, object, pairs, parseJSON, toEncoding, ToJSON, toJSON, Value(Object))
import Data.Map                     (delete, findWithDefault, lookup, Map)
import Data.Monoid                  ((<>))
import Data.List                    (group, maximumBy, minimumBy, sort)
import Data.Function                (on)
import Prelude                      hiding (lookup)

import Dataset                      (Dataset)
import qualified Dataset
import DecisionTree                 (DecisionTree(Answer, Question), DecisionTreeResult(DecisionTreeResult))
import Entropy                      (entropy, informationGain)
import Filter                       (Filter(Filter, NullFilter), parseFilter)
import GetFilters                   (getFilters)
import GetMetaData                  (DataType)
import StatisticalSignificance      (statisticallySignificant)

{- TrainingResult data type -}

data TrainingResult = TrainingResult {
    name                :: String,
    trainingParameters  :: TrainingParameters,
    metaData            :: Map String DataType,
    model               :: DecisionTree
} deriving (Eq, Show)

instance ToJSON TrainingResult where
    toJSON tr = object [
        "name"                  .= name tr,
        "trainingParameters"    .= trainingParameters tr,
        "model"                 .= model tr,
        "metaData"              .= metaData tr
        ]

    toEncoding tr = pairs $
        "name"                  .= name tr <>
        "trainingParameters"    .= trainingParameters tr <>
        "metaData"              .= metaData tr <>
        "model"                 .= model tr

instance FromJSON TrainingResult where
    parseJSON (Object o)    = TrainingResult <$> o .: "name" <*> o .: "trainingParameters" <*> o .: "metaData" <*> o .: "model"
    parseJSON _             = fail "can't parse object"

{- TrainingParameters data type -}

data TrainingParameters = TrainingParameters {
    targetVariable      :: String,
    significanceLevel   :: Double,
    entropyLimit        :: Float
} deriving (Eq, Show)

instance ToJSON TrainingParameters where
    toJSON tp = object [
        "targetVariable"    .= targetVariable tp,
        "significanceLevel" .= significanceLevel tp,
        "entropyLimit"      .= entropyLimit tp
        ]

instance FromJSON TrainingParameters where
    parseJSON (Object o)    = TrainingParameters <$> o .: "targetVariable" <*> o .: "significanceLevel" <*> o .: "entropyLimit"
    parseJSON _             = fail "can't parse object"


{- TrainingResult functions -}

train :: TrainingParameters -> Dataset -> Either String TrainingResult
train trainingParameters dataset = do
    let metaData = delete (targetVariable trainingParameters) $ Dataset.parameters dataset
    let modelName = Dataset.name dataset ++ "_" ++ targetVariable trainingParameters
    model <- trainModel trainingParameters (Dataset.content dataset)
    return $ TrainingResult modelName trainingParameters metaData model

{- DecisionTree construction functions -}

trainModel :: TrainingParameters -> [Map String Value] -> Either String DecisionTree
trainModel _ []                             = Left "can't train based on empty data set"
trainModel trainingParameters trainingData  = do
    let potentialFilters = getFilters $ map (delete $ targetVariable trainingParameters) trainingData
    chosenFilter <- bestFilter trainingParameters trainingData potentialFilters
    constructTree trainingParameters trainingData chosenFilter

constructTree :: TrainingParameters -> [Map String Value] -> Filter -> Either String DecisionTree
constructTree trainingParameters trainingData fil =
    if informationGain trainingData (parseFilter fil) < entropyLimit trainingParameters
        then constructAnswer (targetVariable trainingParameters) trainingData
        else
            affirmativeTree `par` (negativeTree `pseq` Question fil <$> affirmativeTree <*> negativeTree)
            where   affirmativeTree   = trainModel trainingParameters passedTData `using` rdeepseq
                    negativeTree      = trainModel trainingParameters failedTData `using` rdeepseq
                    passedTData       = filter (parseFilter fil) trainingData
                    failedTData       = filter (not . parseFilter fil) trainingData



constructAnswer :: String -> [Map String Value] -> Either String DecisionTree
constructAnswer _ []        = Left "can't construct Answer from empty data set"
constructAnswer targetVariable rawData = do
    pureData <- extractTrainingData targetVariable rawData
    let value = head . maximumBy (compare `on` length) . group . sort $ pureData
    let sampleSize = length pureData
    let confidence = (/ fromIntegral sampleSize) . fromIntegral . length . filter (==value) $ pureData
    return . Answer $ DecisionTreeResult value confidence sampleSize

{- filter evaluating functions -}

bestFilter :: TrainingParameters -> [Map String Value] -> [Filter] -> Either String Filter
bestFilter trainingParameters trainingData potentialFilters = do
    let goodFilters = filterFilters trainingParameters trainingData potentialFilters
    let eitherEntropies = map calculateEntropy goodFilters `using` parListChunk 4 rdeepseq
    filterEntropyPairs <- traverse id eitherEntropies
    let (_, bestFilter) = minimumBy (compare `on` fst) filterEntropyPairs
    return bestFilter
        where   calculateEntropy = getEntropy trainingData $ targetVariable trainingParameters

getEntropy :: [Map String Value] -> String -> Filter -> Either String (Float, Filter)
getEntropy values targetVariable f = do
    filteredValues <- extractTrainingData targetVariable $ filter (parseFilter f) values
    return (entropy filteredValues, f)

extractTrainingData :: String -> [Map String a] -> Either String [a]
extractTrainingData targetVariable = maybe missingKey Right . mapM (lookup targetVariable)
    where   missingKey  = Left $ "missing targetVariable in data: " ++ targetVariable

filterFilters :: TrainingParameters -> [Map String Value] -> [Filter] -> [Filter]
filterFilters trainingParameters trainingData = (NullFilter:) . filter isStatisticallySignificant . filter hasAnyMatches
    where   hasAnyMatches               = not . null . flip filter trainingData . parseFilter
            isStatisticallySignificant  = statisticallySignificant (significanceLevel trainingParameters) trainingData (targetVariable trainingParameters) . parseFilter

