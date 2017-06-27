{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Filter (
    Filter(..),
    parseFilter
) where

import Data.Aeson.Types
import qualified Data.Map

data Filter = NullFilter | Filter {
    operator    :: String,
    value       :: Value,
    key         :: String
} deriving (Eq, Show)

instance ToJSON Filter where
    toJSON f = object [
        "operator"  .= operator f,
        "value"     .= value f,
        "key"       .= key f ]

instance FromJSON Filter where
    parseJSON = withObject "filter" $ \f -> do
        operator    <- f .: "operator"
        value       <- f .: "value"
        key         <- f .: "key"
        return Filter{..}

parseFilter :: Filter -> Data.Map.Map String Value -> Bool
parseFilter NullFilter _ = True
parseFilter f sample
    | operator f == "="     = maybe False (== value f) $ Data.Map.lookup (key f) sample
    | operator f == "!="    = maybe False (/= value f) $ Data.Map.lookup (key f) sample
    | operator f == "<"     = maybe False id $ do
        sampleValue <- Data.Map.lookup (key f) sample
        parsedValue <- parseNumber sampleValue
        filterValue <- parseNumber (value f)
        return (parsedValue < filterValue)
    | operator f == ">"     = maybe False id $ do
        sampleValue <- Data.Map.lookup (key f) sample
        parsedValue <- parseNumber sampleValue
        filterValue <- parseNumber (value f)
        return (parsedValue > filterValue)
    | otherwise             = error $ "invalid filter operator: " ++ operator f

parseNumber :: Value -> Maybe Float
parseNumber = parseMaybe parseJSON
