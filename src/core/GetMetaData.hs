{-# LANGUAGE OverloadedStrings #-}

module GetMetaData (
    structureData,
    getDataTypes,
    DataType(..)
) where

import Data.Aeson   (FromJSON, parseJSON, toJSON, ToJSON, Value(Null, Number, String))
import Data.List    (group, sort)
import Data.Map     (empty, Map, unionWith)

instance Ord Value where
    compare (String a) (String b)   = compare a b
    compare (Number a) (Number b)   = compare a b
    compare (String _) (Number _)   = GT
    compare (Number _) (String _)   = LT
    compare Null Null               = EQ
    compare Null _                  = LT
    compare _ Null                  = GT
    compare _ _                     = EQ

{- structureData functions -}

structureData :: [Map String Value] -> Map String [Value]
structureData = fmap rmdups . foldl (unionWith (++)) empty . map (fmap (:[]))

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

{- getDataTypes functions -}

data DataType = StringType | NumberType | NullType deriving (Eq, Show)

instance ToJSON DataType where
    toJSON StringType = "STRING"
    toJSON NumberType = "NUMBER"

instance FromJSON DataType where
    parseJSON (String "STRING") = return StringType
    parseJSON (String "NUMBER") = return NumberType

getDataTypes :: Map String [Value] -> Map String DataType
getDataTypes = fmap getDataType . fmap setDataTypes
    where   getDataType     = foldl prioritizeType NumberType
            setDataTypes    = map typeCheck

typeCheck :: Value -> DataType
typeCheck (String x)    = StringType
typeCheck (Number x)    = NumberType
typeCheck Null          = NullType

prioritizeType :: DataType -> DataType -> DataType
prioritizeType NumberType NumberType    = NumberType
prioritizeType NullType x               = x
prioritizeType x NullType               = x
prioritizeType _ _                      = StringType
