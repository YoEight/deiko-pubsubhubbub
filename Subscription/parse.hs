{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Subscription.Parse where

import Prelude hiding (head, tail, null)

import Hub.Params
import Subscription.Util

import Control.Applicative
import Control.Monad.State.Class

import Data.Bifunctor
import qualified Data.ByteString as B
import Data.ByteString.Char8 hiding (tail, head, null)
import Data.Either
import Data.Maybe
import Data.Machine
import qualified Data.Text as T
import Data.Text.Encoding

import Text.Parsec.ByteString
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Prim hiding ((<|>))
import Text.ParserCombinators.Parsec.Char

-- Yields a ReqParam according to param name. Also performs
-- basic validation on param value
select :: B.ByteString
          -> B.ByteString
          -> Maybe ReqParam 
select name value
  | name == hub_callback      = either (const Nothing) (Just . Callback) (parseUrl value)
  | name == hub_mode          = either (const Nothing) (Just . Mode) (parseReqType value)
  | name == hub_topic         = either (const Nothing) (Just . Topic) (parseUrl value)
  | name == hub_lease_seconds = either (const Nothing) (Just . LeaseSeconds) (parseInt value)
  | name == hub_secret        = Just $ Secret $ decodeUtf8 value
  | name == hub_verify_token  = Just $ VerifyToken $ decodeUtf8 value
  | name == hub_verify        = either (const Nothing) (Just . Verify) (parseStrategy value)
  | otherwise                 = Nothing

parseParam :: Process (B.ByteString, Maybe B.ByteString) ReqParam
parseParam = repeatedly go
  where
    go = do
      (name, v) <- await
      case v of
        Just value -> maybe (return ()) yield (select name value)
        Nothing    -> return ()

buildRequest :: MonadState Report m
                => ProcessT m ReqParam SubReq
buildRequest = repeatedly $ go Nothing Nothing Nothing Nothing []
  where
    go c@(Just cal) m@(Just mod) t@(Just top) v@(Just ver) opts = do
      x <- await <|> yield (SubReq cal mod top ver opts) *> stop
      go c m t v (x:opts)
    go c m t v opts = do
      param <- await <|> (reportError ClientSide "imcomplete request")
      case param of
        r@(Callback _) -> go (Just r) m t v opts
        r@(Mode _)     -> go c (Just r) t v opts
        r@(Topic _)    -> go c m (Just r) v opts
        r@(Verify _)   -> go c m t (Just r) opts
        r              -> go c m t v (r:opts)
        
parseUrl :: B.ByteString -> Either String B.ByteString
parseUrl input = bimap show (const input) (parse parser "" input) 
  where
    parser = do
      string "http"
      string "s://" <|> string "://"
      some (alphaNum <|> oneOf "-_?/&.") <?> "invalid character"
      eof

parseInt :: B.ByteString -> Either String Int
parseInt input = bimap show id (parse parser "" input)
  where  parser = read <$> some digit <* eof 

parseReqType :: B.ByteString -> Either String ReqType
parseReqType input = bimap show id (parse parser "" input)
  where
    parser = (string "subscribe" *> pure Subscribe) <|> (string "unsubscribe" *> pure Unsubscribe)

parseStrategy :: B.ByteString -> Either String Strategy
parseStrategy input = bimap show id (parse parser "" input)
  where  parser = (string "sync" *> pure Sync) <|> (string "async" *> pure Async) 

parseRequest :: MonadState Report m
                => ProcessT m (B.ByteString, Maybe B.ByteString) SubReq
parseRequest = buildRequest <~ parseParam

hub_callback :: B.ByteString
hub_callback = "hub.callback"

hub_mode :: B.ByteString
hub_mode = "hub.mode"

hub_topic :: B.ByteString
hub_topic = "hub.topic"

hub_verify :: B.ByteString
hub_verify = "hub.verify"

hub_lease_seconds :: B.ByteString
hub_lease_seconds = "hub.lease_seconds"

hub_secret :: B.ByteString
hub_secret = "hub.secret"

hub_verify_token :: B.ByteString
hub_verify_token = "hub.verify_token"
