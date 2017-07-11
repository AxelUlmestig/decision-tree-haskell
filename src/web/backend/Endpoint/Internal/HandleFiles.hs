{-# LANGUAGE OverloadedStrings #-}

module Endpoint.Internal.HandleFiles (
    get,
    getAll,
    delete
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

class (ToJSON a) => Storable a where
    match :: Text -> a -> Bool
    fileName :: a -> FilePath

{- get functions -}

get :: String -> Text -> IO Response
get dir fileName = handle handler $ respond200 <$> LazyBS.readFile filePath
    where   filePath    = dir ++ "/" ++ unpack fileName ++ ".json"
            handler     = (\_ -> return (respond404 "file not found")) :: IOError -> IO Response

{- get all functions -}

getAll :: (ToJSON a, FromJSON a) => (LazyBS.ByteString -> Maybe a) -> String -> IO Response
getAll decodeF dir = respond200 . encode <$> getAllInternal decodeF dir

getAllInternal :: (ToJSON a, FromJSON a) => (LazyBS.ByteString -> Maybe a) -> String -> IO [a]
getAllInternal decodeF dir = do
    fileNames <- listDirectory dir
    files <- mapM (LazyBS.readFile . (dir++)) fileNames
    let parsed = maybe [] id . sequence . filter isJust $ map decodeF files
    return parsed

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

delete :: (FromJSON a, ToJSON a) => String -> (LazyBS.ByteString -> Maybe a) -> (a -> FilePath) -> (a -> Bool) -> IO Response
delete dir decodeF getFileName match = do
    deletionCandidates <- getAllInternal decodeF dir
    let notFoundResponse = Left $ respond404 "could not delete object because it could not be found"
    let deletionTarget = maybe notFoundResponse Right $ find match deletionCandidates
    let remaining = filter (not . match) deletionCandidates
    eitherDeleted <- deleteTarget dir getFileName deletionTarget
    let eitherDeleteResponse = DeleteResponse remaining <$> eitherDeleted
    let eitherResponse = respond200 . encode <$> eitherDeleteResponse
    either return return eitherResponse

deleteTarget :: String -> (a -> FilePath) -> Either Response a -> IO (Either Response a)
deleteTarget dir _ (Left response) = return (Left response)
deleteTarget dir getFileName (Right target) = do
    let path = dir ++ getFileName target
    removeFile path
    return (Right target)

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
