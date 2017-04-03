module Main where
import Test.Framework

import qualified FilterTest
import qualified EntropyTest

tests =     FilterTest.tests ++
            EntropyTest.tests

main = defaultMain tests
