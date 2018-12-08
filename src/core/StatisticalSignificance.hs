
module StatisticalSignificance (
    statisticallySignificant
) where

import Data.List                            (nub)
import Data.Map                             (empty, fromList, insertWith, lookup, Map, unionWith)
import Data.Maybe                           (isJust)
import Prelude                              hiding (fromList, lookup)
import Statistics.Distribution              (complCumulative)
import Statistics.Distribution.ChiSquared   (chiSquared, ChiSquared)

statisticallySignificant :: (Ord a, Ord b) => Double -> [Map a b] -> a -> (Map a b -> Bool) -> Bool
statisticallySignificant limit values key fil =
    if uniqueValues key values < 2
        then False
        else limit > complCumulative distribution chi2
    where   distribution    = getChi2Distribution key values
            chi2            = getChi2 values key fil

getChi2Distribution :: (Ord a, Ord b) => a -> [Map a b] -> ChiSquared
getChi2Distribution key = chiSquared . fromIntegral . pred . uniqueValues key

uniqueValues :: (Ord a, Ord b) => a -> [Map a b] -> Int
uniqueValues key = maybe 0 (length . nub) . traverse (lookup key)

getChi2 :: (Ord a, Ord b) => [Map a b] -> a -> (Map a b -> Bool) -> Double
getChi2 values key fil  = foldl (+) 0 $ unionWith f filtered normalized
    where   f           = \o e -> (o - e)^2 / e
            raw         = extractValues key values
            emptyKeys   = fmap (\_ -> 0) raw
            filtered    = unionWith (+) emptyKeys . extractValues key $ filter fil values
            normFactor  = foldl (+) 0 filtered / foldl (+) 0 raw
            normalized  = fmap (* normFactor) raw

extractValues :: (Ord a, Ord b) => a -> [Map a b] -> Map b Double
extractValues key = maybe empty toMap . traverse (lookup key)
    where   toMap = foldl (\m key -> insertWith (+) key 1 m) empty
