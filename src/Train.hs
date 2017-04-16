{-# LANGUAGE OverloadedStrings #-}

module Train (
    train
) where

import Data.Aeson
import Data.Map (delete, findWithDefault, Map)
import qualified Data.Map (lookup)
import Data.List (group, maximumBy, minimumBy)
import Data.Maybe (fromMaybe)
import Data.Function (on)
import Control.Applicative

import DecisionTree
import Filter
import GetFilters
import Entropy

entropyLimit = 0.2

train :: [Map String Value] -> String -> Either String DecisionTree
train [] _      = Left "can't train based on empty data set"
train tData key = fil >>= constructTree tData key
    where   fil = bestFilter tData key . getFilters . map (delete key) $ tData

constructTree :: [Map String Value] -> String -> Filter -> Either String DecisionTree
constructTree tData key fil =
    if ig > entropyLimit
        then Question fil <$> posTree <*> negTree
        else constructAnswer key tData
    where   ig          = informationGain tData . parseFilter $ fil
            posTree     = train passedTData key
            negTree     = train failedTData key
            passedTData = filter (parseFilter fil) tData
            failedTData = filter (not . parseFilter fil) tData

bestFilter :: [Map String Value] -> String -> [Filter] -> Either String Filter
bestFilter tData key = fmap getLowestEntropy . applyGetEntropy . filter hasAnyMatches
    where   hasAnyMatches       = (>0) . length . flip filter tData . parseFilter
            applyGetEntropy     = sequence . map (getEntropy tData key)
            getLowestEntropy    = snd . minimumBy compareEntropy
            compareEntropy      = compare `on` fst

getEntropy :: [Map String Value] -> String -> Filter -> Either String (Float, Filter)
getEntropy values key f = makePair <$> filteredEntropy f
    where   filteredEntropy = fmap entropy . extractData key . flip filter values . parseFilter
            makePair e      = (e, f)

extractData :: String -> [Map String a] -> Either String [a]
extractData key = fromMaybe (Left ("missing key in data: " ++ key)) . fmap Right . sequence . map (Data.Map.lookup key)

constructAnswer :: String -> [Map String Value] -> Either String DecisionTree
constructAnswer _ [] = Left "can't construct Answer from empty data set"
constructAnswer key rawData = DecisionTreeResult <$> v <*> c <*> ss <**> return Answer
    where   v           = head . maximumBy (compare `on` length) . group <$> pureData
            c           = filter . (==) <$> v <*> pureData <**> return ((/) . fromIntegral . length) <*> fmap fromIntegral ss
            ss          = length <$> pureData
            pureData    = extractData key rawData
