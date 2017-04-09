module Main where
import Test.Framework

import qualified FilterTest
import qualified EntropyTest
import qualified DecisionTreeTest
import qualified TrainTest

tests =     FilterTest.tests ++
            EntropyTest.tests ++
            TrainTest.tests ++
            DecisionTreeTest.tests

main = defaultMain tests
