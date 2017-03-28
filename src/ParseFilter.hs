{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ParseFilter (
    Filter(..),
    parseFilter
) where

import Data.Aeson
import Data.Aeson.Types
import Data.Monoid
import qualified Data.Map

data Filter = Filter {
    operator    :: String,
    value       :: Value,
    key         :: String
}

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

instance Show Filter where
    show f = key f ++ " " ++ operator f ++ " '" ++ show (value f) ++ "'"

instance Eq Filter where
    (==) f1 f2 = (operator f1 == operator f2) && (value f1 == value f2) && (key f1 == key f2)

mbBool :: Maybe Bool -> Bool
mbBool (Just True)  = True
mbBool _            = False

parseFilter :: Filter -> Data.Map.Map String Value -> Bool
parseFilter f values
    | operator f == "="     = mbBool $ (==) <$> Data.Map.lookup (key f) values <*> return (value f)
    | operator f == "!="    = mbBool $ (/=) <$> Data.Map.lookup (key f) values <*> return (value f)
    | operator f == "<"     = mbBool $ (<) <$> (parseNumber =<< Data.Map.lookup (key f) values) <*> (parseNumber . value $ f)
    | operator f == ">"     = mbBool $ (<) <$> (parseNumber =<< Data.Map.lookup (key f) values) <*> (parseNumber . value $ f)
    | otherwise             = False

parseString :: Value -> Maybe String
parseString = parseMaybe parseJSON

parseNumber :: Value -> Maybe Float
parseNumber = parseMaybe parseJSON
