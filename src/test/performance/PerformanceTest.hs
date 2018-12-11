
--module PerformanceTest () where

import Control.Parallel     (par, pseq)
import Data.Maybe           (fromJust)
import Data.Time
import Data.Aeson           (decode)
import Dataset
import Train         hiding (trainingParameters)
import qualified Data.ByteString.Lazy.Char8 as LazyBS

--result :: Either String TrainingResult
--result = train trainingParameters dataset


main :: IO ()
main = do
    trainingData <- dataset
    let result = train trainingParameters trainingData
    t0 <- getCurrentTime
    pseq result (return ())
    t1 <- getCurrentTime
    print ("sum: " ++ ((:[]) . last $ show result))
    print ("time: " ++ show (diffUTCTime t1 t0))

trainingParameters = TrainingParameters "Survived" 0.05 0.2 :: TrainingParameters
dataset = fromJust . decode <$> LazyBS.readFile "./datasets/performance-test-dataset.json" :: IO Dataset
