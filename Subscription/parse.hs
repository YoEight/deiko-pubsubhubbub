{-# LANGUAGE NoMonomorphismRestriction, TupleSections, TypeFamilies #-}

module Subscription.Parse (parseRequest) where

import Prelude hiding (head, tail, null)

import Subscription.Params

import Control.Applicative
import Control.Monad
import Control.Pipe

import Data.Bifunctor
import qualified Data.ByteString as B
import Data.ByteString.Char8 hiding (tail, head, null)
import Data.Either
import Data.Foldable hiding (all)
import Data.ListLike hiding (all)
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding

import Text.Parsec.ByteString
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Prim hiding ((<|>))
import Text.ParserCombinators.Parsec.Char

stripPrefix :: B.ByteString
               -> B.ByteString
               -> Maybe B.ByteString
stripPrefix prefix value
  | null value  = Nothing
  | null prefix = Just value
  | otherwise   = let matched = (head prefix) == (head value)
                  in if matched then stripPrefix (tail prefix) (tail value)
                     else Nothing

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

parseParam :: Monad m =>
              Pipe (Maybe (B.ByteString, B.ByteString)) (Maybe ReqParam) m r
parseParam = forever $ await >>= go
  where
    go (Just (name, value)) = case select name value of
      Nothing -> return ()
      r       -> yield r
    go _                    = yield Nothing

-- Discard param request that doesn't start by 'hub.'   
filterParam :: Monad m =>
               Pipe (Maybe (B.ByteString, B.ByteString)) (Maybe (B.ByteString, B.ByteString)) m ()
filterParam = forever $ await >>= go
  where
    go (Just (name, value)) = case stripPrefix hub_prefix name of
      Just stripped -> yield $ Just (stripped, value)
      Nothing       -> return ()
    go _ = yield Nothing

buildRequest :: Monad m =>
                Pipe (Maybe ReqParam) (Either String Req) m ()
buildRequest = go Nothing Nothing Nothing Nothing []
  where
    go c@(Just callback) m@(Just mode) t@(Just topic) v@(Just verify) optionals = do
      param <- await
      maybe (yield $ Right $ Req callback mode topic verify optionals) (go c m t v . (:optionals)) param

    go c m t v optionals = do
      param <- await
      case param of
        Nothing -> (yield . Left) "invalid request"
        Just p  -> case p of
          r@(Callback _) -> go (Just r) m t v optionals
          r@(Mode _)     -> go c (Just r) t v optionals
          r@(Topic _)    -> go c m (Just r) v optionals
          r@(Verify _)   -> go c m t (Just r) optionals
          r              -> go c m t v (r:optionals)

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

parseRequest :: Monad m =>
                Pipe (Maybe (ByteString, ByteString)) (Either String Req) m ()
parseRequest = buildRequest <+< parseParam <+< filterParam

hub_prefix = B.pack [104,117,98,46]

hub_callback = B.pack [99,97,108,108,98,97,99,107]

hub_mode = B.pack [109,111,100,101]

hub_topic = B.pack [116,111,112,105,99]

hub_verify = B.pack [118,101,114,105,102,121]

hub_lease_seconds = B.pack [108,101,97,115,101,95,115,101,99,111,110,100,115]

hub_secret = B.pack [115,101,99,114,101,116]

hub_verify_token = B.pack [118,101,114,105,102,121,95,116,111,107,101,110]
