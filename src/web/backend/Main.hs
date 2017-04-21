{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Blaze.ByteString.Builder.Char.Utf8 (fromString)
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

import qualified Endpoint.Dataset
import qualified Endpoint.Index

notImplemented = responseBuilder status418 [("Content-Type", "text/text")] (fromString "not implemented")

getResponse :: Request -> IO Response
getResponse Request{pathInfo="api":"dataset":setName:[], requestMethod="PUT", requestBody=body}     = Endpoint.Dataset.put setName body
getResponse Request{pathInfo="api":"dataset":setName:[], requestMethod="GET"}                       = Endpoint.Dataset.get setName
getResponse Request{pathInfo="api":"dataset":setName:"train":[], requestMethod="POST"}              = return $ notImplemented
getResponse Request{pathInfo="api":"model":modelName:[], requestMethod="GET"}                       = return $ notImplemented
getResponse Request{pathInfo="api":"model":modelName:"evaluate":[], requestMethod="POST"}           = return $ notImplemented
getResponse Request{rawPathInfo="/", requestMethod="GET"}                                           = Endpoint.Index.get
getResponse _ = return $ responseBuilder status404 [("Content-Type", "text/text")] (fromString "four oh four")

application :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
--application req respond = getResponse req >>= respond
application = (>>=) . getResponse

main = run 3000 application
