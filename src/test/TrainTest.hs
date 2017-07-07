{-# LANGUAGE OverloadedStrings #-}

module TrainTest (
    tests
) where

import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Aeson
import Data.Map
import Control.Applicative
import Data.Either (isLeft)

import Dataset (prepareDataset)
import DecisionTree
import Train

createDataset = prepareDataset "model name"

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
infixl 4 <&>

significanceLevel = 1.0

train1 = TestCase $ assertEqual "one training sample" expected actual
    where   actual      = flip askTree sample <$> tree <&> answer
            expected    = return value
            tree        = model <$> train significanceLevel dataset key
            dataset     = createDataset [sample]
            sample      = fromList [("other key", String "other value"), (key, value)]
            key         = "key"
            value       = String "value"

train2 = TestCase $ assertEqual "two training samples, first option" expected actual
    where   expected    = return k1v1
            actual      = flip askTree (singleton k2 k2v1) <$> tree <&> answer
            tree        = model <$> train significanceLevel (createDataset [sample1, sample2]) k1
            sample1     = fromList [(k2, k2v1), (k1, k1v1)]
            sample2     = fromList [(k2, k2v2), (k1, k1v2)]
            k1          = "k1"
            k2          = "k2"
            k1v1        = String "key 1, value 1"
            k1v2        = String "key 1, value 2"
            k2v1        = String "key 2, value 1"
            k2v2        = String "key 2, value 2"

train3 = TestCase $ assertEqual "two training samples, second option" expected actual
    where   expected    = return k1v2
            actual      = flip askTree (singleton k2 k2v2) <$> tree <&> answer
            tree        = model <$> train significanceLevel (createDataset [sample1, sample2]) k1
            sample1     = fromList [(k2, k2v1), (k1, k1v1)]
            sample2     = fromList [(k2, k2v2), (k1, k1v2)]
            k1          = "k1"
            k2          = "k2"
            k1v1        = String "key 1, value 1"
            k1v2        = String "key 1, value 2"
            k2v1        = String "key 2, value 1"
            k2v2        = String "key 2, value 2"

train4 = TestCase $ assertEqual "three training samples" expected actual
    where   expected    = return k1v2
            actual      = flip askTree (singleton k2 k2v1) <$> tree <&> answer
            tree        = model <$> train significanceLevel (createDataset [sample1, sample2, sample3]) k1
            sample1     = fromList [(k2, k2v1), (k1, k1v1)]
            sample2     = fromList [(k2, k2v1), (k1, k1v2)]
            sample3     = fromList [(k2, k2v1), (k1, k1v2)]
            k1          = "k1"
            k2          = "k2"
            k1v1        = String "key 1, value 1"
            k1v2        = String "key 1, value 2"
            k2v1        = String "key 2, value 1"

train5 = TestCase $ assertEqual "single training sample, number" expected actual
    where   expected    = return k1v
            actual      = flip askTree (singleton k2 k2v) <$> tree <&> answer
            tree        = model <$> train significanceLevel dataset k1
            dataset     = createDataset [fromList [(k2, k2v), (k1, k1v)]]
            k1          = "k1"
            k2          = "k2"
            k1v         = Number 1
            k2v         = Number 2

train6 = TestCase $ assertEqual "positive and negative number" expected actual
    where   expected    = return k1v1
            actual      = flip askTree (singleton k2 (Number 10)) <$> tree <&> answer
            tree        = model <$> train significanceLevel (createDataset [sample1, sample2, sample3, sample4]) k1
            k1          = "k1"
            k2          = "k2"
            sample1     = fromList [(k2, k2v1), (k1, k1v2)]
            sample2     = fromList [(k2, k2v2), (k1, k1v2)]
            sample3     = fromList [(k2, k2v3), (k1, k1v1)]
            sample4     = fromList [(k2, k2v4), (k1, k1v1)]
            k1v1        = String "positive"
            k1v2        = String "negative"
            k2v1        = Number (-2)
            k2v2        = Number (-1)
            k2v3        = Number 1
            k2v4        = Number 2

train7 = TestCase $ assertEqual "positive and negative number" expected actual
    where   expected    = return k1v2
            actual      = flip askTree (singleton k2 (Number (-10))) <$> tree <&> answer
            tree        = model <$> train significanceLevel (createDataset [sample1, sample2, sample3, sample4]) k1
            k1          = "k1"
            k2          = "k2"
            sample1     = fromList [(k2, k2v1), (k1, k1v2)]
            sample2     = fromList [(k2, k2v2), (k1, k1v2)]
            sample3     = fromList [(k2, k2v3), (k1, k1v1)]
            sample4     = fromList [(k2, k2v4), (k1, k1v1)]
            k1v1        = String "positive"
            k1v2        = String "negative"
            k2v1        = Number (-2)
            k2v2        = Number (-1)
            k2v3        = Number 1
            k2v4        = Number 2

train8 = TestCase $ assertBool "train modelName on empty dataset" (isLeft tree)
    where   tree = train significanceLevel (createDataset []) "key"

train9 = TestCase $ assertBool "train modelName on non-existing key" (isLeft tree)
    where   tree    = train significanceLevel dataset k1
            dataset = createDataset [singleton k2 k2v]
            k1      = "key 1"
            k2      = "key 2"
            k2v     = "value"


labels = ["train" ++ show n | n <- [1..]]

testCases = [
        train1,
        train2,
        train3,
        train4,
        train5,
        train6,
        train7,
        train8,
        train9
    ]

tests = hUnitTestToTests . TestList . zipWith TestLabel labels $ testCases
