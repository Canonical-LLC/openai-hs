module OpenAI.Client.Internal.SSE where

import Network.HTTP.Client 
import OpenAI.Resources
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Attoparsec.ByteString.Char8
import           Control.Applicative 
import           Control.Monad (when)

event :: Parser ServerEvent
event = (sevent <|> comment <|> retry) <* eol

sevent :: Parser ServerEvent
sevent = ServerEvent
  <$> optional (string "event" *> char ':' *> chars <* eol)
  <*> optional (string "id"    *> char ':' *> chars <* eol)
  <*> fmap BSL.fromChunks (many     (string "data"  *> char ':' *> chars <* eol))

comment :: Parser ServerEvent
comment = CommentEvent <$> (char ':' *> chars <* eol)

retry :: Parser ServerEvent
retry = RetryEvent <$> (string "retry:" *> decimal <* eol)

chars :: Parser BS.ByteString
chars = takeTill (== '\n')

eol :: Parser Char
eol = char '\n'

data ServerEvent
    = ServerEvent {
        eventName :: Maybe BS.ByteString,
        eventId   :: Maybe BS.ByteString,
        eventData :: BSL.ByteString
        }
    | CommentEvent {
        eventComment :: BS.ByteString
        }
    | RetryEvent {
        eventRetry :: Int
        }
    | CloseEvent

-- Make a type called Event for the following json
-- {"id": "cmpl-XXXXXXXXXXXXXXXXXXXXXXX", "object": "text_completion", "created": 1671700494, "choices": [{"text": " way", "index": 0, "logprobs": null, "finish_reason": null}], "model": "text-davinci-003"}
data Event = Event { 
  id :: String,
  object :: String,
  created :: Int,
  choices :: [TextCompletionChoice],
  model :: String
} deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)


withEvents :: Manager -> String ->  (Either String ServerEvent -> IO ()) -> IO ()
withEvents m url f = do 
    -- make a request from the url
  req <- parseRequest url

  let 
    parseLoop :: IO BS.ByteString -> BS.ByteString -> IO ()
    parseLoop source partial = do
        -- get the body of the response
      body <- source
      when (not (BS.null body) || not (BS.null partial)) $ do 
        parseWith source event (partial <> body) >>= \case 
          Done i r -> do 
            f $ Right r
            parseLoop source i
          Partial _ -> f $ Left "Unexpected end of input"
          Fail _ _ e -> f $ Left e
        
  withResponse req m $ \resp -> do 
    let bodyReader = responseBody resp
    parseLoop bodyReader ""