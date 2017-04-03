module Main where
import Test.Framework

import qualified ParseFilterTest
import qualified EntropyTest

tests =     ParseFilterTest.tests ++
            EntropyTest.tests

main = defaultMain tests
