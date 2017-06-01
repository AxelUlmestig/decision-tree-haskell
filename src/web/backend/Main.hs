{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp           (run)
import Network.Wai.Middleware.Static      (addBase, staticPolicy)

import Network.Wai.Internal
import Data.Aeson

import qualified Endpoint.Dataset
import qualified Endpoint.Model
import qualified Endpoint.Index

notImplemented = responseBuilder status418 [("Content-Type", "text/text")] (fromString "not implemented")

getResponse :: Request -> IO Response
getResponse Request{pathInfo=["api","dataset",setName], requestMethod="PUT", requestBody=body}              = Endpoint.Dataset.put setName body
getResponse Request{pathInfo=["api","dataset"], requestMethod="GET"}                                        = Endpoint.Dataset.getAll
getResponse Request{pathInfo=["api","dataset",""], requestMethod="GET"}                                     = Endpoint.Dataset.getAll
getResponse Request{pathInfo=["api","dataset",setName], requestMethod="GET"}                                = Endpoint.Dataset.get setName
getResponse Request{pathInfo=["api","dataset",setName], requestMethod="DELETE"}                             = Endpoint.Dataset.delete setName
getResponse Request{pathInfo=["api","dataset",setName,""], requestMethod="DELETE"}                          = Endpoint.Dataset.delete setName
getResponse Request{pathInfo=["api","dataset",setName,"train"], requestMethod="POST", requestBody=body}     = Endpoint.Dataset.train setName body
getResponse Request{pathInfo=["api","model"], requestMethod="GET"}                                          = Endpoint.Model.getAll
getResponse Request{pathInfo=["api","model",""], requestMethod="GET"}                                       = Endpoint.Model.getAll
getResponse Request{pathInfo=["api","model",modelName], requestMethod="GET"}                                = Endpoint.Model.get modelName
getResponse Request{pathInfo=["api","model",modelName], requestMethod="DELETE"}                             = Endpoint.Model.delete modelName
getResponse Request{pathInfo=["api","model",modelName,"evaluate"], requestMethod="POST", requestBody=body}  = Endpoint.Model.evaluate modelName body
getResponse Request{rawPathInfo="/", requestMethod="GET"}                                                   = Endpoint.Index.get
getResponse _ = return $ responseBuilder status404 [("Content-Type", "text/text")] (fromString "four oh four")

application :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
application = (>>=) . getResponse

main = run 3000 $ staticPolicy (addBase "src/web/frontend/build/") application
