{-# LANGUAGE OverloadedStrings #-}

module GetMetaData (
    structureData,
    getDataTypes,
    DataType(..)
) where

import Data.Aeson
import Data.List (group, sort)
import qualified Data.Map (map)
import Data.Map (empty, Map, unionWith)

instance Ord Value where
    compare v1 v2 = compare (show v1) (show v2)

{- structureData functions -}

structureData :: [Map String Value] -> Map String [Value]
structureData = Data.Map.map rmdups . foldl (unionWith (++)) empty . map (Data.Map.map (:[]))

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

{- getDataTypes functions -}

data DataType = StringType | NumberType deriving (Eq, Show)

instance ToJSON DataType where
    toJSON StringType = "STRING"
    toJSON NumberType = "NUMBER"

instance FromJSON DataType where
    parseJSON (String "STRING") = return StringType
    parseJSON (String "NUMBER") = return NumberType

getDataTypes :: Map String [Value] -> Map String DataType
getDataTypes = Data.Map.map getDataType . Data.Map.map setDataTypes
    where   getDataType     = foldl prioritizeType NumberType
            setDataTypes    = map typeCheck

typeCheck :: Value -> DataType
typeCheck (String x) = StringType
typeCheck (Number x) = NumberType

prioritizeType :: DataType -> DataType -> DataType
prioritizeType NumberType NumberType    = NumberType
prioritizeType _ _                      = StringType
