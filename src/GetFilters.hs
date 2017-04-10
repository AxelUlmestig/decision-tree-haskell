{-# LANGUAGE OverloadedStrings #-}

module GetFilters (
    getFilters,
    parseFilter
) where

import qualified Data.Map
import qualified Data.Set
import qualified Data.List
import Data.Aeson

import Filter

instance Ord Value where
    compare v1 v2 = compare (show v1) (show v2)

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort
    where   sort    = Data.List.sort
            group   = Data.List.group

sortValues :: [Data.Map.Map String Value] -> Data.Map.Map String [Value]
sortValues = mapMap rmdups . foldl (unionWith (++)) empty . map (mapMap (:[]))
    where   empty       = Data.Map.empty
            unionWith   = Data.Map.unionWith
            mapMap      = Data.Map.map

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
getTypeFilters _ values = map (Filter "=") values ++ map (Filter "!=") values

filtersFromDataTypes :: Data.Map.Map String [Value] -> Data.Map.Map String [String -> Filter]
filtersFromDataTypes values = intersectionWith getTypeFilters dataTypes values
    where   dataTypes           = mapMap getDataType values
            intersectionWith    = Data.Map.intersectionWith
            mapMap              = Data.Map.map

bindParametersToFilters :: Data.Map.Map String [String -> Filter] -> [Filter]
bindParametersToFilters = foldl (++) [] . mapWithKey applyKey
    where   applyKey    = map . flip ($)
            mapWithKey  = Data.Map.mapWithKey

getFilters :: [Data.Map.Map String Value] -> [Filter]
getFilters = bindParametersToFilters . filtersFromDataTypes . sortValues
