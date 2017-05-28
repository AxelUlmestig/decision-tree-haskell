{-# LANGUAGE OverloadedStrings #-}

module GetFilters (
    getFilters,
    parseFilter
) where

import qualified Data.Map (map)
import Data.Map (intersectionWith, Map, mapWithKey)
import qualified Data.Set
import Data.List (sortBy)
import Data.Aeson

import Filter
import GetMetaData

getTypeFilters :: DataType -> [Value] -> [String -> Filter]
getTypeFilters StringType values = map (Filter "=") values ++ map (Filter "!=") values
getTypeFilters NumberType values = map (Filter ">") . prune (length values `quot` 3) . sortBy numValue $ values
    where   numValue (Number v1) (Number v2) = compare v1 v2

prune :: Int -> [a] -> [a]
prune _ []      = []
prune n (x:xs)  = x : prune n (drop n xs)

filtersFromDataTypes :: Map String [Value] -> Map String [String -> Filter]
filtersFromDataTypes values = intersectionWith getTypeFilters dataTypes values
    where   dataTypes   = getDataTypes values

bindParametersToFilters :: Map String [String -> Filter] -> [Filter]
bindParametersToFilters = concat . mapWithKey applyKey
    where   applyKey    = map . flip ($)

getFilters :: [Map String Value] -> [Filter]
getFilters = (NullFilter:) . bindParametersToFilters . filtersFromDataTypes . structureData
