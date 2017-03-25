{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Blaze.ByteString.Builder           (fromByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromShow, fromString, fromText)
import Data.Monoid                        ((<>))
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp           (run)

import Network.Wai.Internal
import Data.Text (unpack)
import Control.Exception (SomeException, try)
import qualified Data.ByteString.Char8 as C8
import Data.Aeson
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as LazyBS

getResponse :: Request -> IO Response

getResponse Request{requestMethod="PUT", pathInfo="api":"dataset":setName:[], requestBody=requestBody} = do
    body <- ioBody
    case body of
        Just dataset -> LazyBS.writeFile filePath dataset >> (return $
            responseBuilder status201 [("Content-Type", "text/html")] (fromString ""))
        Nothing -> return $
            responseBuilder status400 [("Content-Type", "text/html")] (fromString "dataset not found")

    where   filePath = "./datasets/" ++ (unpack setName) ++ ".json"
            decodeMap = decode :: LazyBS.ByteString -> Maybe (Map.Map String Value)
            ioBody = (fmap encode) . (>>= Map.lookup "dataset") . decodeMap . LazyBS.fromStrict <$> requestBody

getResponse Request{requestMethod="GET", pathInfo="api":"dataset":setName:[]} = do
    fileContent <- try . readFile $ filePath
    case fileContent of
        Left (_ :: SomeException) -> return $
            responseBuilder status404 [("Content-Type", "text/html")] (fromString "dataset not found")
        Right content -> return $
            responseBuilder status200 [("Content-Type", "text/html")] (fromString content)

    where   filePath = "./datasets/" ++ (unpack setName) ++ ".json"

getResponse Request{requestMethod="GET", rawPathInfo="/"} =
    readFile "./index.html" >>=
    return . responseBuilder status200 [("Content-Type", "text/html")] . fromString

getResponse _ = return $
    responseBuilder status404 [("Content-Type", "text/text")] (fromString "four oh four")

application :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
--application req respond = getResponse req >>= respond
application = (>>=) . getResponse

main = run 3000 application
