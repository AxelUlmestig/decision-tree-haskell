{-# LANGUAGE OverloadedStrings #-}

module StatisticsTest (
    tests
) where

import Test.Framework.Providers.HUnit   (hUnitTestToTests)
import Test.HUnit                       (Test(TestCase, TestLabel, TestList), assertEqual)
import Data.Map                         (fromList, lookup, Map)
import Prelude                          hiding (lookup)

import StatisticalSignificance          (statisticallySignificant)

{-
This test is based on an example from this site
http://www.wikihow.com/Calculate-P-Value

In the example observing 90 red cars and
60 blue cars when the expected ratio is
2:1 is statistically significant with a
0.1 p value but not 0.05.
-}

expectedRedCars = repeat $ fromList [("colour", "red"), ("src", "expected")] :: [Map String String]
expectedBlueCars = repeat $ fromList [("colour", "blue"), ("src", "expected")] :: [Map String String]

observedRedCars = repeat $ fromList [("colour", "red"), ("src", "observed")] :: [Map String String]
observedBlueCars = repeat $ fromList [("colour", "blue"), ("src", "observed")] :: [Map String String]

hasAttribute :: String -> String -> Map String String -> Bool
hasAttribute key value = maybe False (==value) . lookup key

statistics1 = TestCase $ assertEqual "expected: 2/1, observed: 90/60, statistical significance: 0.1" expected actual
    where   expected        = True
            actual          = statisticallySignificant 0.1 cars "colour" isObserved
            cars            = take 90 observedRedCars ++ take 30 expectedRedCars ++ take 60 observedBlueCars
            isObserved      = hasAttribute "src" "observed"

statistics2 = TestCase $ assertEqual "expected: 2/1, observed: 90/60, statistical significance: 0.05" expected actual
    where   expected        = False
            actual          = statisticallySignificant 0.05 cars "colour" isObserved
            cars            = take 90 observedRedCars ++ take 30 expectedRedCars ++ take 60 observedBlueCars
            isObserved      = hasAttribute "src" "observed"

labels = ["statistics" ++ show n | n <- [1..]]

testCases = [
        statistics1,
        statistics2
    ]

tests = hUnitTestToTests . TestList . zipWith TestLabel labels $ testCases
