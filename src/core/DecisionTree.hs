{-# LANGUAGE OverloadedStrings #-}

module DecisionTree (
    DecisionTree(..),
    DecisionTreeResult(..),
    askTree
) where

import Control.DeepSeq      (NFData, rnf)
import Control.Applicative  ((<**>))
import Data.Aeson           ((.:), (.=), FromJSON, object, pairs, parseJSON, toEncoding, ToJSON, toJSON, Value, withObject)
import Data.Map             (Map)
import Data.Monoid          ((<>))
import Data.Foldable        (asum)

import Filter               (Filter, parseFilter)

data DecisionTree =
    Question Filter DecisionTree DecisionTree |
    Answer DecisionTreeResult
    deriving (Eq, Show)

instance ToJSON DecisionTree where
    toJSON (Answer dtr) =
        toJSON dtr
    toJSON (Question f d1 d2) =
        object [
            "question" .= f,
            "positiveResponse" .= d1,
            "negativeResponse" .= d2
        ]

    toEncoding (Answer dtr) =
        toEncoding dtr
    toEncoding (Question f d1 d2) =
        pairs $     "question"          .= f <>
                    "positiveResponse"  .= d1 <>
                    "negativeResponse"  .= d2

instance FromJSON DecisionTree where
    parseJSON = withObject "Question or Answer" $ \dt -> asum [
            DecisionTreeResult <$> dt .: "answer" <*> dt .: "confidence" <*> dt .: "sampleSize" <**> return Answer,
            Question <$> dt .: "question" <*> dt .: "positiveResponse" <*> dt .: "negativeResponse"
        ]

instance NFData DecisionTree where
    rnf (Answer dtr)            = rnf dtr
    rnf (Question fil dt1 dt2)  = rnf fil `seq` rnf dt1 `seq` rnf dt2

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

instance NFData DecisionTreeResult where
    rnf (DecisionTreeResult ans conf ss) = rnf ans `mappend` rnf conf `mappend` rnf ss

askTree :: DecisionTree -> Map String Value -> DecisionTreeResult
askTree (Answer dtr) _ = dtr
askTree (Question f d1 d2) arg =
    if parseFilter f arg
        then askTree d1 arg
        else askTree d2 arg
