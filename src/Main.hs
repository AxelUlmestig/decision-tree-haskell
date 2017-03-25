{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Blaze.ByteString.Builder           (fromByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromShow, fromString, fromText)
import Control.Concurrent.MVar
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

getResponse :: MVar Integer -> Request -> IO Response

getResponse _ Request{requestMethod="GET", pathInfo="api":"pathinfo":rest} = return $
    responseBuilder
        status200
        [("Content-Type", "text/text")]
        (fromString . foldl (++) [] . map (++" ") . map unpack $ rest)

getResponse countRef Request{requestMethod="GET", rawPathInfo="/api/increment"} = do
    updateMVar countRef plus1
    count <- readMVar countRef
    let msg = fromString $ show count
    return $
        responseBuilder
            status404
            [("Content-Type", "text/text")]
            msg

{-
getResponse _ Request{requestMethod="PUT", pathInfo="api":"dataset":setName:[]} =
    requestBody >>=
    return . (>>= Map.lookup "dataset") . decode . LazyBS.fromStrict >>=
    writeFile filePath >>
    responseBuilder
        status201
        [("Content-Type", "text/html")]
        (fromString "")
    where   filePath = "./datasets/" ++ (unpack setName) ++ ".json"
    -}

getResponse _ Request{requestMethod="PUT", pathInfo="api":"dataset":setName:[], requestBody=requestBody} = do
    body <- ioBody
    case body of
        Just dataset -> do
            LazyBS.writeFile filePath dataset
            return $
                responseBuilder
                    status201
                    [("Content-Type", "text/html")]
                    (fromString "")
        Nothing -> return $
            responseBuilder
                status400
                [("Content-Type", "text/html")]
                (fromString "dataset not found")
    where   filePath = "./datasets/" ++ (unpack setName) ++ ".json"
            decodeMap = decode :: LazyBS.ByteString -> Maybe (Map.Map String Value)
            ioBody = (fmap encode) . (>>= Map.lookup "dataset") . decodeMap . LazyBS.fromStrict <$> requestBody

getResponse _ Request{requestMethod="GET", pathInfo="api":"dataset":setName:[]} = do
    fileContent <- try . readFile $ filePath
    case fileContent of
        Left (_ :: SomeException) -> return $
            responseBuilder
                status404
                [("Content-Type", "text/html")]
                (fromString "dataset not found")
        Right content -> return $
            responseBuilder
                status200
                [("Content-Type", "text/html")]
                (fromString content)
    where   filePath = "./datasets/" ++ (unpack setName) ++ ".json"

getResponse _ Request{requestMethod="GET", rawPathInfo="/"} = do
    fileContent <- readFile "./index.html"
    return $
        responseBuilder
            status200
            [("Content-Type", "text/html")]
            (fromString fileContent)

getResponse _ _ = return $
    responseBuilder
        status404
        [("Content-Type", "text/text")]
        (fromString "four oh four")

plus1 = (+1)

updateMVar :: MVar a -> (a -> a) -> IO ()
updateMVar mvar f = do
    x <- takeMVar mvar
    putMVar mvar (f x)

application :: MVar Integer -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
application countRef req respond = do
    response <- getResponse countRef req
    respond response

main = do
    visitorCount <- newMVar 0
    run 3000 $ application visitorCount
