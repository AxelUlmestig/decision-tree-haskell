
module StatisticalSignificance (
    statisticallySignificant
) where

import Data.Aeson
import Data.List
import Data.Map (empty, insertWith, Map, unionWith)
import Data.Maybe (isJust)
import qualified Data.Map (lookup, map, fromList)
import Statistics.Distribution
import Statistics.Distribution.ChiSquared

import Filter

statisticallySignificant :: Ord v => Double -> [Map String v] -> String -> (Map String v -> Bool) -> Bool
statisticallySignificant limit values key fil =
    if uniqueValues key values < 2
        then False
        else limit > complCumulative distribution chi2
    where   distribution    = getChi2Distribution key values
            chi2            = getChi2 values key fil

getChi2Distribution :: Ord v => String -> [Map String v] -> ChiSquared
getChi2Distribution key = chiSquared . fromIntegral . pred . uniqueValues key

uniqueValues :: Ord v => String -> [Map String v] -> Int
uniqueValues key = maybe 0 (length . nub) . mapM (Data.Map.lookup key)

getChi2 :: Ord v => [Map String v] -> String -> (Map String v -> Bool) -> Double
getChi2 values key fil = foldl (+) 0 $ unionWith f filtered normalized
    where   f               = \o e -> (o - e)^2 / e
            raw             = extractValues key values
            emptyKeys       = Data.Map.map (\_ -> 0) raw
            filtered        = unionWith (+) emptyKeys . extractValues key $ filter fil values
            normFactor      = foldl (+) 0 filtered / foldl (+) 0 raw
            normalized      = Data.Map.map (* normFactor) raw

extractValues :: Ord v => String -> [Map String v] -> Map v Double
extractValues key = maybe empty toMap . mapM (Data.Map.lookup key)
    where   toMap   = foldl (\m key -> insertWith (+) key 1 m) empty
