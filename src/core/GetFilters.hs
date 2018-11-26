{-# LANGUAGE OverloadedStrings #-}

module GetFilters (
    getFilters
) where

import Data.Map     (intersectionWith, mapWithKey, Map)
import Data.List    (sortBy)
import Data.Aeson   (Value(Number))

import Filter       (Filter(Filter, NullFilter), Operator(EqOperator, GtOperator, NeqOperator))
import GetMetaData  (DataType(NumberType, StringType), getDataTypes, structureData)

getTypeFilters :: DataType -> [Value] -> [String -> Filter]
getTypeFilters StringType values = map (Filter EqOperator) values ++ map (Filter NeqOperator) values
getTypeFilters NumberType values = map (Filter GtOperator) . prune (length values `quot` 3) . sortBy numValue $ values
    where   numValue (Number v1) (Number v2) = compare v1 v2
            numValue _ _ = GT

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
