{-# LANGUAGE OverloadedStrings #-}

import Blaze.ByteString.Builder           (fromByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromShow, fromString, fromText)
import Control.Concurrent.MVar
import Data.Monoid                        ((<>))
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp           (run)

import Network.Wai.Internal

import Data.Text (unpack)

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
