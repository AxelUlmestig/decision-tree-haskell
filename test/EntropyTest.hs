{-# LANGUAGE OverloadedStrings #-}

module EntropyTest (
    tests
) where

import Test.Framework.Providers.HUnit
import Test.HUnit

import Entropy

entropy1 = TestCase $ assertEqual "no data" (entropy arr) 0.0
    where   arr = [] :: [Int]

entropy2 = TestCase $ assertEqual "no entropy" (entropy [1]) 0.0

entropy3 = TestCase $ assertEqual "no entropy" (entropy [1, 1]) 0.0

entropy4 = TestCase $ assertBool "positive entropy" positiveEntropy
    where   positiveEntropy = 0 < (entropy [1,2])

labels = ["entropy" ++ show n | n <- [1..]]

testCases = [
        entropy1,
        entropy2,
        entropy3,
        entropy4
    ]

tests = hUnitTestToTests . TestList . zipWith TestLabel labels $ testCases
