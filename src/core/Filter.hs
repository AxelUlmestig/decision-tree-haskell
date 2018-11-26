{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Filter (
    Filter(..),
    Operator(..),
    parseFilter
) where

import Control.DeepSeq      (NFData, rnf)
import Data.Aeson.Types
import Data.Map             (Map, lookup)
import Prelude              hiding (Map, lookup)

data Filter = NullFilter | Filter {
    operator    :: Operator,
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

data Operator =
    EqOperator  |
    NeqOperator |
    GtOperator  |
    LtOperator
    deriving (Eq, Show)

instance ToJSON Operator where
    toJSON EqOperator  = "="
    toJSON NeqOperator = "!="
    toJSON GtOperator  = ">"
    toJSON LtOperator  = "<"

instance FromJSON Operator where
    parseJSON "="   = return EqOperator
    parseJSON "!="  = return NeqOperator
    parseJSON ">"   = return GtOperator
    parseJSON "<"   = return LtOperator
    parseJSON _     = fail ""

instance NFData Filter where
    rnf NullFilter      = ()
    rnf (Filter _ v k) = (rnf v) `mappend` (rnf k)

parseFilter :: Filter -> Map String Value -> Bool
parseFilter f = maybe False id . parseFilterInternal f

parseFilterInternal :: Filter -> Map String Value -> Maybe Bool
parseFilterInternal NullFilter _                    = Just True
parseFilterInternal (Filter EqOperator val key) sample     = (== val) <$> lookup key sample
parseFilterInternal (Filter NeqOperator val key) sample    = (/= val) <$> lookup key sample
parseFilterInternal (Filter LtOperator val key) sample     = do
    sampleValue <- lookup key sample
    parsedValue <- parseNumber sampleValue
    filterValue <- parseNumber val
    return (parsedValue < filterValue)
parseFilterInternal (Filter GtOperator val key) sample     = do
    sampleValue <- lookup key sample
    parsedValue <- parseNumber sampleValue
    filterValue <- parseNumber val
    return (parsedValue > filterValue)

parseNumber :: Value -> Maybe Float
parseNumber = parseMaybe parseJSON
