module Entropy (
    entropy,
    informationGain
) where

entropy :: Eq a => [a] -> Float
entropy values = negate . sum . map f $ values
    where   f value = pmf (==value) values * logBase 2 (pmf (==value) values)

pmf :: (a -> Bool) -> [a] -> Float
pmf predicate values = filteredLength / rawLength
    where   filteredLength  = fromIntegral . length . filter predicate $ values
            rawLength       = fromIntegral . length $ values

informationGain :: Eq a => [a] -> (a -> Bool) -> Float
informationGain values fil = before - after
    where   before = entropy values
            after = entropy . filter fil $ values
