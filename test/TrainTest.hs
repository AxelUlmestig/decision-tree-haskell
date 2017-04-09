{-# LANGUAGE OverloadedStrings #-}

module TrainTest (
    tests
) where

import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Aeson
import Data.Map

import DecisionTree
import Train

train1 = TestCase $ assertEqual "one training sample" value actual
    where   actual  = answer $ askTree tree sample
            tree    = train [sample] key
            sample  = insert "otherKey" "ost" $ singleton key value
            key     = "key"
            value   = String "value"

train2 = TestCase $ assertEqual "two training samples, first option" expected actual
    where   expected    = k1v1
            actual      = answer $ askTree tree (singleton k2 k2v1)
            tree        = train [sample1, sample2] k1
            sample1     = insert k2 k2v1 $ singleton k1 k1v1
            sample2     = insert k2 k2v2 $ singleton k1 k1v2
            k1          = "k1"
            k2          = "k2"
            k1v1        = (String "key 1, value 1")
            k1v2        = (String "key 1, value 2")
            k2v1        = (String "key 2, value 1")
            k2v2        = (String "key 2, value 2")

train3 = TestCase $ assertEqual "two training samples, second option" expected actual
    where   expected    = k1v2
            actual      = answer $ askTree tree (singleton k2 k2v2)
            tree        = train [sample1, sample2] k1
            sample1     = insert k2 k2v1 $ singleton k1 k1v1
            sample2     = insert k2 k2v2 $ singleton k1 k1v2
            k1          = "k1"
            k2          = "k2"
            k1v1        = (String "key 1, value 1")
            k1v2        = (String "key 1, value 2")
            k2v1        = (String "key 2, value 1")
            k2v2        = (String "key 2, value 2")

train4 = TestCase $ assertEqual "three training samples" expected actual
    where   expected    = k1v2
            actual      = answer $ askTree tree (singleton k2 k2v1)
            tree        = train [sample1, sample2, sample3] k1
            sample1     = insert k2 k2v1 $ singleton k1 k1v1
            sample2     = insert k2 k2v1 $ singleton k1 k1v2
            sample3     = insert k2 k2v1 $ singleton k1 k1v2
            k1          = "k1"
            k2          = "k2"
            k1v1        = (String "key 1, value 1")
            k1v2        = (String "key 1, value 2")
            k2v1        = (String "key 2, value 1")


labels = ["train" ++ show n | n <- [1..]]

testCases = [
        train1,
        train2,
        train3,
        train4
    ]

tests = hUnitTestToTests . TestList . zipWith TestLabel labels $ testCases
