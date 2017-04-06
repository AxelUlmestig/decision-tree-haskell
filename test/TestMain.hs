module Main where
import Test.Framework

import qualified FilterTest
import qualified EntropyTest
import qualified DecisionTreeTest

tests =     FilterTest.tests ++
            EntropyTest.tests ++
            DecisionTreeTest.tests

main = defaultMain tests
