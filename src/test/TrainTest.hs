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

sigLevel = 1.0
entLimit = 0.2

train1 = TestCase $ assertEqual "one training sample" expected actual
    where   actual              = flip askTree sample <$> tree <&> answer
            expected            = return value
            trainingParameters  = TrainingParameters key sigLevel entLimit
            tree                = model <$> train trainingParameters dataset
            dataset             = createDataset [sample]
            sample              = fromList [("other key", String "other value"), (key, value)]
            key                 = "key"
            value               = String "value"

train2 = TestCase $ assertEqual "two training samples, first option" expected actual
    where   expected            = return k1v1
            actual              = flip askTree (singleton k2 k2v1) <$> tree <&> answer
            trainingParameters  = TrainingParameters k1 sigLevel entLimit
            tree                = model <$> train trainingParameters (createDataset [sample1, sample2])
            sample1             = fromList [(k2, k2v1), (k1, k1v1)]
            sample2             = fromList [(k2, k2v2), (k1, k1v2)]
            k1                  = "k1"
            k2                  = "k2"
            k1v1                = String "key 1, value 1"
            k1v2                = String "key 1, value 2"
            k2v1                = String "key 2, value 1"
            k2v2                = String "key 2, value 2"

train3 = TestCase $ assertEqual "two training samples, second option" expected actual
    where   expected            = return k1v2
            actual              = flip askTree (singleton k2 k2v2) <$> tree <&> answer
            trainingParameters  = TrainingParameters k1 sigLevel entLimit
            tree                = model <$> train trainingParameters (createDataset [sample1, sample2])
            sample1             = fromList [(k2, k2v1), (k1, k1v1)]
            sample2             = fromList [(k2, k2v2), (k1, k1v2)]
            k1                  = "k1"
            k2                  = "k2"
            k1v1                = String "key 1, value 1"
            k1v2                = String "key 1, value 2"
            k2v1                = String "key 2, value 1"
            k2v2                = String "key 2, value 2"

train4 = TestCase $ assertEqual "three training samples" expected actual
    where   expected            = return k1v2
            actual              = flip askTree (singleton k2 k2v1) <$> tree <&> answer
            trainingParameters  = TrainingParameters k1 sigLevel entLimit
            tree                = model <$> train trainingParameters (createDataset [sample1, sample2, sample3])
            sample1             = fromList [(k2, k2v1), (k1, k1v1)]
            sample2             = fromList [(k2, k2v1), (k1, k1v2)]
            sample3             = fromList [(k2, k2v1), (k1, k1v2)]
            k1                  = "k1"
            k2                  = "k2"
            k1v1                = String "key 1, value 1"
            k1v2                = String "key 1, value 2"
            k2v1                = String "key 2, value 1"

train5 = TestCase $ assertEqual "single training sample, number" expected actual
    where   expected            = return k1v
            actual              = flip askTree (singleton k2 k2v) <$> tree <&> answer
            trainingParameters  = TrainingParameters k1 sigLevel entLimit
            tree                = model <$> train trainingParameters dataset
            dataset             = createDataset [fromList [(k2, k2v), (k1, k1v)]]
            k1                  = "k1"
            k2                  = "k2"
            k1v                 = Number 1
            k2v                 = Number 2

train6 = TestCase $ assertEqual "positive and negative number" expected actual
    where   expected            = return k1v1
            actual              = flip askTree (singleton k2 (Number 10)) <$> tree <&> answer
            trainingParameters  = TrainingParameters k1 sigLevel entLimit
            tree                = model <$> train trainingParameters (createDataset [sample1, sample2, sample3, sample4])
            k1                  = "k1"
            k2                  = "k2"
            sample1             = fromList [(k2, k2v1), (k1, k1v2)]
            sample2             = fromList [(k2, k2v2), (k1, k1v2)]
            sample3             = fromList [(k2, k2v3), (k1, k1v1)]
            sample4             = fromList [(k2, k2v4), (k1, k1v1)]
            k1v1                = String "positive"
            k1v2                = String "negative"
            k2v1                = Number (-2)
            k2v2                = Number (-1)
            k2v3                = Number 1
            k2v4                = Number 2

train7 = TestCase $ assertEqual "positive and negative number" expected actual
    where   expected            = return k1v2
            actual              = flip askTree (singleton k2 (Number (-10))) <$> tree <&> answer
            trainingParameters  = TrainingParameters k1 sigLevel entLimit
            tree                = model <$> train trainingParameters (createDataset [sample1, sample2, sample3, sample4])
            k1                  = "k1"
            k2                  = "k2"
            sample1             = fromList [(k2, k2v1), (k1, k1v2)]
            sample2             = fromList [(k2, k2v2), (k1, k1v2)]
            sample3             = fromList [(k2, k2v3), (k1, k1v1)]
            sample4             = fromList [(k2, k2v4), (k1, k1v1)]
            k1v1                = String "positive"
            k1v2                = String "negative"
            k2v1                = Number (-2)
            k2v2                = Number (-1)
            k2v3                = Number 1
            k2v4                = Number 2

train8 = TestCase $ assertBool "train modelName on empty dataset" (isLeft tree)
    where   trainingParameters  = TrainingParameters "key" sigLevel entLimit
            tree                = train trainingParameters (createDataset [])

train9 = TestCase $ assertBool "train modelName on non-existing key" (isLeft tree)
    where   trainingParameters  = TrainingParameters k1 sigLevel entLimit
            tree                = train trainingParameters dataset
            dataset             = createDataset [singleton k2 k2v]
            k1                  = "key 1"
            k2                  = "key 2"
            k2v                 = "value"

train10 = TestCase $ assertEqual "extract most common output from dataset 1" expected actual
    where   expected            = Right "b"
            actual              = answer . flip askTree Data.Map.empty . model <$> tree
            trainingParameters  = TrainingParameters key 0.01 entLimit
            tree                = train trainingParameters trainingDataset
            trainingDataset     = createDataset [
                                    singleton key (String "a"),
                                    singleton key (String "b"),
                                    singleton key (String "c"),
                                    singleton key (String "b")
                                ]
            key                 = "key"

train11 = TestCase $ assertEqual "extract most common output from dataset 2" expected actual
    where   expected            = Right "a"
            actual              = answer . flip askTree Data.Map.empty . model <$> tree
            trainingParameters  = TrainingParameters key 0.01 entLimit
            tree                = train trainingParameters trainingDataset
            trainingDataset     = createDataset [
                                    singleton key (String "a"),
                                    singleton key (String "b"),
                                    singleton key (String "a"),
                                    singleton key (String "c")
                                ]
            key                 = "key"

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
        train9,
        train10,
        train11
    ]

tests = hUnitTestToTests . TestList . zipWith TestLabel labels $ testCases
