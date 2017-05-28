{-# LANGUAGE OverloadedStrings #-}

module DecisionTree (
    DecisionTree(..),
    DecisionTreeResult(..),
    askTree
) where

import Data.Aeson
import Data.Map (Map)
import Data.Foldable (asum)
import Control.Applicative

import Filter

data DecisionTree = Question Filter DecisionTree DecisionTree | Answer DecisionTreeResult
    deriving (Eq, Show)

instance ToJSON DecisionTree where
    toJSON (Answer dtr) =
        toJSON dtr
    toJSON (Question f d1 d2) =
        object [
            "question" .= toJSON f,
            "positiveResponse" .= toJSON d1,
            "negativeResponse" .= toJSON d2
        ]

instance FromJSON DecisionTree where
    parseJSON = withObject "Question or Answer" $ \dt -> asum [
            DecisionTreeResult <$> dt .: "answer" <*> dt .: "confidence" <*> dt .: "sampleSize" <**> return Answer,
            Question <$> dt .: "question" <*> dt .: "positiveResponse" <*> dt .: "negativeResponse"
        ]

data DecisionTreeResult = DecisionTreeResult {
    answer      :: Value,
    confidence  :: Float,
    sampleSize  :: Int
} deriving (Eq, Show)

instance ToJSON DecisionTreeResult where
    toJSON (DecisionTreeResult v c ss) =
        object [
            "answer" .= toJSON v,
            "confidence" .= toJSON c,
            "sampleSize" .= toJSON ss
        ]

instance FromJSON DecisionTreeResult where
    parseJSON = withObject "DecisionTreeResult" $ \o ->
        DecisionTreeResult <$> o .: "answer" <*> o .: "confidence" <*> o .: "sampleSize"

askTree :: DecisionTree -> Map String Value -> DecisionTreeResult
askTree (Answer dtr) _ = dtr
askTree (Question f d1 d2) arg =
    if parseFilter f arg
        then askTree d1 arg
        else askTree d2 arg
