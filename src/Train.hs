{-# LANGUAGE OverloadedStrings #-}

module Train (
    train
) where

import Data.Aeson
import Data.Map (delete, findWithDefault, Map)
import Data.List (group, maximumBy, minimumBy)
import Data.Maybe (fromMaybe)
import Data.Function (on)

import DecisionTree
import Filter
import GetFilters
import Entropy

entropyLimit = 0.2

train :: [Map String Value] -> String -> DecisionTree
train [] _      = error "can't train based on empty data set"
train tData key = if ig > entropyLimit
    then Question fil posTree negTree
    else constructAnswer key tData
    where   fil         = bestFilter tData key . getFilters . map (delete key) $ tData
            ig          = informationGain tData (parseFilter fil)
            posTree     = train passedTData key
            negTree     = train failedTData key
            passedTData = filter (parseFilter fil) tData
            failedTData = filter (not . parseFilter fil) tData

bestFilter :: [Map String Value] -> String -> [Filter] -> Filter
bestFilter tData key = minimumBy compareEntropy . filter filterFilter
    where   compareEntropy  = compare `on` filteredEntropy
            filteredEntropy = entropy . extractData key . flip filter tData . parseFilter
            filterFilter    = (>0) . length . flip filter tData . parseFilter

extractData :: String -> [Map String a] -> [a]
extractData = map . findWithDefault (error "missing value")

constructAnswer :: String -> [Map String Value] -> DecisionTree
constructAnswer _ [] = error "can't construct Answer from empty data set"
constructAnswer key rawData = Answer $ DecisionTreeResult v c ss
    where   v           = head . maximumBy (compare `on` length) . group $ pureData
            c           = (fromIntegral . length . filter (==v) $ pureData) / fromIntegral ss
            ss          = length pureData
            pureData    = extractData key rawData
