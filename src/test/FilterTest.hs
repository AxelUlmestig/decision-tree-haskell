{-# LANGUAGE OverloadedStrings #-}

module FilterTest (
    tests
) where

import Test.Framework.Providers.HUnit
import Test.HUnit

import Data.Aeson
import Data.Map (Map, singleton, empty)

import Filter

parse1 = TestCase $ assertBool "match equals string" expression
    where   expression  = parseFilter fil obj           :: Bool
            fil         = Filter EqOperator (String "b") "a"   :: Filter
            obj         = singleton "a" (String "b")    :: Map String Value

parse2 = TestCase $ assertBool "match equals integer" expression
    where   expression  = parseFilter fil obj           :: Bool
            fil         = Filter EqOperator (Number 1) "a"     :: Filter
            obj         = singleton "a" (Number 1)      :: Map String Value

parse3 = TestCase $ assertBool "match equals float" expression
    where   expression  = parseFilter fil obj           :: Bool
            fil         = Filter EqOperator (Number 1.1) "a"   :: Filter
            obj         = singleton "a" (Number 1.1)    :: Map String Value

parse4 = TestCase $ assertEqual "mismatch equals string" False expression
    where   expression  = parseFilter fil obj           :: Bool
            fil         = Filter EqOperator (String "b") "a"   :: Filter
            obj         = singleton "a" (String "c")    :: Map String Value

parse5 = TestCase $ assertEqual "mismatch equals integer" False expression
    where   expression  = parseFilter fil obj           :: Bool
            fil         = Filter EqOperator (Number 1) "a"     :: Filter
            obj         = singleton "a" (Number 2)      :: Map String Value

parse6 = TestCase $ assertEqual "mismatch equals float" False expression
    where   expression  = parseFilter fil obj           :: Bool
            fil         = Filter EqOperator (Number 1.1) "a"   :: Filter
            obj         = singleton "a" (Number 2.1)    :: Map String Value

parse7 = TestCase $ assertEqual "missing value equals" False expression
    where   expression  = parseFilter fil obj           :: Bool
            fil         = Filter EqOperator (String "b") "a"   :: Filter
            obj         = empty                         :: Map String Value

parse8 = TestCase $ assertEqual "missing value greater than" False expression
    where   expression  = parseFilter fil obj           :: Bool
            fil         = Filter GtOperator (Number 1) "a"     :: Filter
            obj         = empty                         :: Map String Value

parse9 = TestCase $ assertEqual "from JSON equal" f1 f2
    where   f1 = Just $ Filter EqOperator (String "b") "a"                          :: Maybe Filter
            f2 = decode "{\"operator\": \"=\", \"key\": \"a\", \"value\": \"b\"}"   :: Maybe Filter

parse10 = TestCase $ assertBool "from JSON not equal" (f1 /= f2)
    where   f1 = Just $ Filter EqOperator (String "c") "a"                          :: Maybe Filter
            f2 = decode "{\"operator\": \"=\", \"key\": \"a\", \"value\": \"b\"}"   :: Maybe Filter

parse11 = TestCase $ assertBool "from JSON not equal" (f1 /= f2)
    where   f1 = Just $ Filter EqOperator (Number 1) "a"                            :: Maybe Filter
            f2 = decode "{\"operator\": \"=\", \"key\": \"a\", \"value\": \"b\"}"   :: Maybe Filter

parse12 = TestCase $ assertBool "from JSON not equal" (f1 /= f2)
    where   f1 = Just $ Filter LtOperator (String "b") "a"                          :: Maybe Filter
            f2 = decode "{\"operator\": \"=\", \"key\": \"a\", \"value\": \"b\"}"   :: Maybe Filter

parse13 = TestCase $ assertEqual "greater than, True" True expression
    where   expression  = parseFilter fil obj               :: Bool
            fil         = Filter GtOperator (Number 1) "a"  :: Filter
            obj         = singleton "a" (Number 2)          :: Map String Value

parse14 = TestCase $ assertEqual "greater than, False" False expression
    where   expression  = parseFilter fil obj               :: Bool
            fil         = Filter GtOperator (Number 1) "a"  :: Filter
            obj         = singleton "a" (Number 0)          :: Map String Value

parse15 = TestCase $ assertEqual "lesser than, True" True expression
    where   expression  = parseFilter fil obj               :: Bool
            fil         = Filter LtOperator (Number 1) "a"  :: Filter
            obj         = singleton "a" (Number 0)          :: Map String Value

parse16 = TestCase $ assertEqual "lesser than, False" False expression
    where   expression  = parseFilter fil obj               :: Bool
            fil         = Filter LtOperator (Number 1) "a"  :: Filter
            obj         = singleton "a" (Number 2)          :: Map String Value


labels = ["parse" ++ show n | n <- [1..]]

testCases = [
        parse1,
        parse2,
        parse3,
        parse4,
        parse5,
        parse6,
        parse7,
        parse8,
        parse9,
        parse10,
        parse11,
        parse12,
        parse13,
        parse14,
        parse15,
        parse16
    ]

tests = hUnitTestToTests . TestList . zipWith TestLabel labels $ testCases
