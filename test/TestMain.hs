module Main where
import Test.Framework

import qualified ParseFilterTest

tests =     ParseFilterTest.tests

main = defaultMain tests
