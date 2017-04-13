{-# LANGUAGE OverloadedStrings #-}

module GetFilters (
    getFilters,
    parseFilter
) where

import qualified Data.Map (map)
import Data.Map (empty, intersectionWith, Map, mapWithKey, unionWith)
import qualified Data.Set
import Data.List (group, sort, sortBy)
import Data.Aeson

import Filter

mapMap = Data.Map.map

instance Ord Value where
    compare v1 v2 = compare (show v1) (show v2)

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

sortValues :: [Map String Value] -> Map String [Value]
sortValues = mapMap rmdups . foldl (unionWith (++)) empty . map (mapMap (:[]))

number = "Number"
string = "String"

typeCheck :: Value -> String
typeCheck (String x) = string
typeCheck (Number x) = number

prioritizeType :: String -> String -> String
prioritizeType "Number" "Number" = number
prioritizeType _ _ = string

getDataType :: [Value] -> String
getDataType = foldl prioritizeType number . map typeCheck

getTypeFilters :: String -> [Value] -> [String -> Filter]
getTypeFilters "String" values = map (Filter "=") values ++ map (Filter "!=") values
getTypeFilters "Number" values = map (Filter ">") . prune (length values `quot` 3) . sortBy numValue $ values
    where   numValue (Number v1) (Number v2) = compare v1 v2

prune :: Int -> [a] -> [a]
prune _ [] = []
prune n (x:xs) = x : prune n (drop n xs)

filtersFromDataTypes :: Map String [Value] -> Map String [String -> Filter]
filtersFromDataTypes values = intersectionWith getTypeFilters dataTypes values
    where   dataTypes           = mapMap getDataType values

bindParametersToFilters :: Map String [String -> Filter] -> [Filter]
bindParametersToFilters = foldl (++) [] . mapWithKey applyKey
    where   applyKey    = map . flip ($)

getFilters :: [Map String Value] -> [Filter]
getFilters = (NullFilter:) . bindParametersToFilters . filtersFromDataTypes . sortValues
