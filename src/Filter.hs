{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Filter (
    Filter(..),
    parseFilter
) where

import Data.Aeson
import Data.Aeson.Types
import Data.Monoid
import qualified Data.Map
import Data.Maybe (fromMaybe)

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
parseFilter f values
    | operator f == "="     = fromMaybe False . fmap (== value f) . Data.Map.lookup (key f) $ values
    | operator f == "!="    = fromMaybe False . fmap (/= value f) . Data.Map.lookup (key f) $ values
    | operator f == "<"     = fromMaybe False $ (<) <$> (parseNumber =<< Data.Map.lookup (key f) values) <*> (parseNumber . value $ f)
    | operator f == ">"     = fromMaybe False $ (>) <$> (parseNumber =<< Data.Map.lookup (key f) values) <*> (parseNumber . value $ f)
    | otherwise             = error $ "invalid filter operator: " ++ operator f

parseString :: Value -> Maybe String
parseString = parseMaybe parseJSON

parseNumber :: Value -> Maybe Float
parseNumber = parseMaybe parseJSON
