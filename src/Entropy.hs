module Entropy (
    entropy,
    informationGain
) where

entropy :: Eq a => [a] -> Float
entropy xs = negate . sum . map f $ xs
    where   f x = pmf (==x) xs * logBase 2 (pmf (==x) xs)

pmf :: (a -> Bool) -> [a] -> Float
pmf f v = filteredLength / rawLength
    where   filteredLength  = fromIntegral . length . filter f $ v
            rawLength       = fromIntegral . length $ v

informationGain :: Eq a => [a] -> (a -> Bool) -> Float
informationGain xs fil = before - after
    where   before = entropy xs
            after = entropy . filter fil $ xs
