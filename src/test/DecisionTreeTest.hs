{-# LANGUAGE OverloadedStrings #-}

module DecisionTreeTest (
    tests
) where

import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Aeson
import Data.Map

import DecisionTree
import Filter

decisionTree1 = TestCase $ assertBool "parse DecisionTreeResult" (parsed /= Nothing)
    where   parsed  = decode "{\"answer\": \"ost\", \"confidence\": 1, \"sampleSize\": 1}" :: Maybe DecisionTreeResult

decisionTree2 = TestCase $ assertEqual "single answer from json" (Just . String $ "ost") result
    where   result  = fmap answer $ askTree <$> tree <*> return empty :: Maybe Value
            tree    = decode "{\"answer\": \"ost\", \"confidence\": 1, \"sampleSize\": 1}" :: Maybe DecisionTree

decisionTree3 = TestCase $ assertEqual "single answer" response answerValue
    where   response    = answer . askTree tree $ empty
            tree        = Answer $ DecisionTreeResult answerValue 1 1
            answerValue = String "ost"

decisionTree4 = TestCase $ assertEqual "two options, pass filter" expected actual
    where   actual      = askTree tree $ singleton "key" "value"
            expected    = posAnswer
            tree        = Question fil (Answer posAnswer) (Answer negAnswer)
            fil         = Filter "=" (String "value") "key"
            posAnswer   = DecisionTreeResult (String "pos answer") 1 1
            negAnswer   = DecisionTreeResult (String "neg answer") 1 1

decisionTree5 = TestCase $ assertEqual "two layers" expected actual
    where   actual      = askTree tree $ singleton "key" "value"
            tree        = Question fil tree2 (Answer negAnswer)
            tree2       = Question fil (Answer expected) (Answer negAnswer)
            fil         = Filter "=" (String "value") "key"
            expected    = DecisionTreeResult (String "pos answer") 1 1
            negAnswer   = DecisionTreeResult (String "neg answer") 1 1

labels = ["decisionTree" ++ show n | n <- [1..]]

testCases = [
        decisionTree1,
        decisionTree2,
        decisionTree3,
        decisionTree4,
        decisionTree5
    ]

tests = hUnitTestToTests . TestList . zipWith TestLabel labels $ testCases
