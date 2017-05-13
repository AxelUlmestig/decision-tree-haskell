{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Dataset (
    prepareDataset,
    Dataset(..)
) where

import Data.Aeson
import Data.Map

import GetMetaData

data Dataset = Dataset {
    name        :: String,
    content     :: [Map String Value],
    parameters  :: Map String DataType
}

instance ToJSON Dataset where
    toJSON ds = object [
        "name"          .= name ds,
        "content"       .= content ds,
        "parameters"    .= parameters ds
        ]

instance FromJSON Dataset where
    parseJSON = withObject "dataset" $ \ds -> do
        name        <- ds .: "name"
        content     <- ds .: "content"
        parameters  <- ds .: "parameters"
        return Dataset{..}

prepareDataset :: String -> [Map String Value] -> Dataset
prepareDataset name rawData = Dataset name rawData $ getDataTypes (structureData rawData)
