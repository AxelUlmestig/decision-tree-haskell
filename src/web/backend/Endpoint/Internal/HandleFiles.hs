{-# LANGUAGE OverloadedStrings #-}

module Endpoint.Internal.HandleFiles (
    get,
    getAll,
    save,
    delete,
    Storable(..),
    DeleteResponse
) where

import Control.Exception (handle)
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LazyBS
import Data.List (find)
import qualified Data.Map as Map
import Data.Map (Map, singleton)
import Data.Maybe (isJust)
import Data.Text (Text, unpack)
import Network.HTTP.Types
import Network.Wai
import System.Directory

class (ToJSON a, FromJSON a) => Storable a where
    match :: String -> a -> Bool
    fileName :: a -> FilePath

{- get functions -}

get :: Storable a => String -> String -> IO (Either String a)
get dir objectName = do
    potentialMatches <- getAll dir
    let matches = filter (match objectName) potentialMatches
    case matches of
        [] -> return $ Left "file not found"
        (storable:_) -> return $ Right storable

{- get all functions -}

getAll :: Storable a => String -> IO [a]
getAll dir = do
    fileNames <- listDirectory dir
    files <- mapM (LazyBS.readFile . (dir++)) fileNames
    let parsed = maybe [] id . sequence . filter isJust $ map decode files
    return parsed

{- save functions -}

save :: Storable a => String -> a -> IO a
save dir storable = do
    let path = dir ++ "/" ++ fileName storable
    LazyBS.writeFile path (encode storable)
    return storable

{- delete functions -}

data DeleteResponse a = DeleteResponse {
    remaining   :: [a],
    deleted     :: a
}

instance (ToJSON a) => ToJSON (DeleteResponse a) where
    toJSON dr = object [
            "deleted"   .= deleted dr,
            "remaining" .= remaining dr
        ]

delete :: Storable a => String -> String -> IO (Either String (DeleteResponse a))
delete dir objectName = do
    eitherDeleteTarget <- get dir objectName
    case eitherDeleteTarget of
        Left err -> return $ Left err
        Right target -> do
            removeFile $ dir ++ fileName target
            remaining <- getAll dir
            return . Right $ DeleteResponse remaining target

{- misc functions -}

formatError :: String -> LazyBS.ByteString
formatError = encodeMap . singleton "err"
    where   encodeMap = encode :: Map String String -> LazyBS.ByteString

appJsonHeader :: ResponseHeaders
appJsonHeader = [("Content-Type", "application/json")]

respond200 = responseLBS status200 appJsonHeader
respond201 = responseLBS status201 appJsonHeader
respond400 = responseLBS status400 appJsonHeader . formatError
respond404 = responseLBS status404 appJsonHeader . formatError
respond500 = responseLBS status500 appJsonHeader . formatError
