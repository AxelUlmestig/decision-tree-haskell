module Main where
import Test.Framework

import qualified FilterTest
import qualified EntropyTest
import qualified DecisionTreeTest
import qualified TrainTest
import qualified StatisticsTest

tests =     FilterTest.tests ++
            EntropyTest.tests ++
            TrainTest.tests ++
            DecisionTreeTest.tests ++
            StatisticsTest.tests

main = defaultMain tests
