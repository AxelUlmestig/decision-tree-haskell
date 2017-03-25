{-# LANGUAGE OverloadedStrings #-}

import Data.Map
import Data.Aeson

data DataSetRequest = DataSetRequest Data.Aeson.Value

ints = decode "[{\"foo\":1,\"bar\":\"ost\"}]" :: Maybe ([Map String Value])

main = do
    putStrLn . show . fmap (Data.Map.lookup "bar") . fmap head $ ints
