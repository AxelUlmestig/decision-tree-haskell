{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import qualified Data.Text as T
import qualified Data.Foldable as F
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V
import Data.Aeson

import qualified Data.Attoparsec as P
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Types as DAT
import qualified Data.String as S

data JSONEvent = JSONEvent [(String, String)] (Maybe String) deriving Show

instance FromJSON JSONEvent where
  parseJSON = parseJSONEvent

decodedEvent = decode "{\"name\":\"edwald\",\"args\":[{\"a\": \"b\"}]}" :: Maybe JSONEvent
decodedEvent2 = decode "{\"name\":\"edwald\",\"args\":[{\"a\": \"b\"}, {\"c\": \"d\"}]}" :: Maybe JSONEvent
decodedEvent3 = decode "{\"name\":\"edwald\",\"args\":[{\"a\": \"b\", \"c\": \"d\"}]}" :: Maybe JSONEvent

emptyAesonArray :: Value
emptyAesonArray = Array $ V.fromList []

parseJSONEvent :: Value -> Parser JSONEvent
parseJSONEvent v =
  case v of
    Object o -> do
      name <- o .:? "name"
      argsJSON <- o .:? "args" .!= emptyAesonArray
      case argsJSON of
        Array m -> do
          parsedList <- V.toList <$> V.mapM (parseJSON :: Value -> Parser (HM.HashMap T.Text Value)) m
          let parsedCatList = concatMap HM.toList parsedList
          args <- mapM (\(key, value) -> (,) <$> (return (T.unpack key)) <*> (parseJSON :: Value -> Parser String) value) parsedCatList
          return $ JSONEvent args name
        _ -> fail ((show argsJSON) ++ " is not an Array.")
    _ -> fail ((show v) ++ " is not an Object.")

-- Useful for debugging aeson parsers
decodeWith :: (Value -> Parser b) -> String -> Either String b
decodeWith p s = do
  value <- P.eitherResult $ (P.parse json . S.fromString) s
  DAT.parseEither p value
