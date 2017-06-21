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
import qualified Data.Map as Map
import Data.Map (lookup, Map, singleton)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text, unpack)
import Network.HTTP.Types
import Network.Wai
import System.Directory

class (ToJSON a) => Storable a where
    match :: Text -> a -> Bool
    fileName :: a -> FilePath

{- get functions -}

get :: String -> Text -> IO Response
get dir fileName = handle handler $ respond200 . LazyBS.pack <$> readFile filePath
    where   filePath    = dir ++ "/" ++ unpack fileName ++ ".json"
            handler     = (\_ -> return (respond404 "file not found")) :: IOError -> IO Response

{- get all functions -}

getAll :: (ToJSON a, FromJSON a) => (LazyBS.ByteString -> Maybe a) -> String -> IO Response
getAll decodeF dir = respond200 . encode . extractDatasets decodeF <$> getAllRaw dir

getAllInternal :: FromJSON a => (LazyBS.ByteString -> Maybe a) -> String -> IO [a]
getAllInternal decodeF dir = extractDatasets decodeF <$> getAllRaw dir

getAllRaw :: String -> IO [String]
getAllRaw dir = listDirectory dir >>= mapM readDataset
    where   readDataset = readFile . (dir++)

extractDatasets :: FromJSON a => (LazyBS.ByteString -> Maybe a) -> [String] -> [a]
extractDatasets decodeF = fromMaybe [] . sequence . filter isJust . map (decodeF . LazyBS.pack)

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
delete dir decodeF getFileName match = formatDeleteResponse <$> (getAllInternal decodeF dir >>= deleteDataset dir getFileName . deleteInternal match)

deleteInternal :: (ToJSON a) => (a -> Bool) -> [a] -> Either String (DeleteResponse a)
deleteInternal match datasets = DeleteResponse remaining <$> deleted
    where   remaining   = filter (not . match) datasets
            deleted     = eitherHead errMessage $ filter match datasets
            errMessage  = "could not delete object because it could not be found"

deleteDataset :: (ToJSON a) => String -> (a -> FilePath) -> Either String (DeleteResponse a)-> IO (Either String (DeleteResponse a))
deleteDataset dir getFileName (Left err)    = return (Left err)
deleteDataset dir getFileName (Right dr)    = removeFile filePath >> return (Right dr)
    where   filePath    = dir ++ getFileName (deleted dr)

formatDeleteResponse :: (ToJSON a) => Either String (DeleteResponse a) -> Response
formatDeleteResponse (Left err) = respond404 err
formatDeleteResponse (Right dr) = respond200 . encode $ dr

{- misc functions -}

eitherHead :: a -> [b] -> Either a b
eitherHead err [] = Left err
eitherHead _ (x:xs) = Right x

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
