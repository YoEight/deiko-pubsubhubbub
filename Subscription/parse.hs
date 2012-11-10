{-# LANGUAGE NoMonomorphismRestriction, TupleSections, TypeFamilies #-}

module Subscription.Parse where

import Prelude hiding (head, tail, null)

import Subscription.Params

import Control.Monad
import Control.Monad.Trans.Class
import Control.Proxy

import qualified Data.ByteString as B
import Data.Foldable
import Data.ListLike
import Data.Maybe
import qualified Data.Text as T

source :: (Monad m, Foldable f) =>
          f a
          -> Pipe () a m ()
source = traverse_ yield

sourceMaybe :: (Monad m, Foldable f) =>
               f a
               -> Pipe () (Maybe a) m ()
sourceMaybe fa = traverse_ (yield . Just) fa >> yield Nothing


stripPrefix :: B.ByteString
               -> B.ByteString
               -> Maybe B.ByteString
stripPrefix prefix value
  | null value  = Nothing
  | null prefix = Just value
  | otherwise   = let matched = (head prefix) == (head value)
                  in if matched then stripPrefix (tail prefix) (tail value)
                     else Nothing

-- Yields a SubParam according to param name. Also performs
-- basic validation on param value
select :: B.ByteString
          -> B.ByteString
          -> Maybe SubParam 
select name value
  | name == hub_callback      = Just $ Callback T.empty
  | name == hub_mode          = Just $ Mode T.empty
  | name == hub_topic         = Just $ Topic T.empty
  | name == hub_lease_seconds = Just $ LeaseSeconds 0
  | name == hub_secret        = Just $ Secret T.empty
  | name == hub_verify_token  = Just $ VerifyToken T.empty
  | name == hub_verify        = Just $ Verify Sync
  | otherwise                 = Nothing

parseParam :: Monad m =>
              Pipe (Maybe (B.ByteString, B.ByteString)) (Maybe SubParam) m r
parseParam = pipe (uncurry select =<<)

-- Discard param request that doesn't start by 'hub.'   
filterParam :: Monad m =>
               Pipe (Maybe (B.ByteString, B.ByteString)) (Maybe (B.ByteString, B.ByteString)) m r
filterParam = pipe go
  where go pair = do
          (name, value) <- pair
          stripped      <- stripPrefix hub_prefix name
          return (stripped, value)

buildRequest :: Monad m =>
                Pipe (Maybe SubParam) SubReq m ()
buildRequest = go []
  where
    go xs = do
      param <- await
      maybe (yield $ SubReq xs) (go . (:xs)) param

printer = forever ((lift . print) =<< await)

-- Simulate Arrow.first 
first :: (Monad m, r ~ ()) =>
         Pipe a b m r
         -> Pipe (a, c) (b, c) m r
first inner = forever $ ((\(a, c) -> ((yield . (, c)) =<< await) <+< inner <+< yield a) =<< await)

hub_prefix = B.pack [104,117,98,46]

hub_callback = B.pack [99,97,108,108,98,97,99,107]

hub_mode = B.pack [109,111,100,101]

hub_topic = B.pack [116,111,112,105,99]

hub_verify = B.pack [118,101,114,105,102,121]

hub_lease_seconds = B.pack [108,101,97,115,101,95,115,101,99,111,110,100,115]

hub_secret = B.pack [115,101,99,114,101,116]

hub_verify_token = B.pack [118,101,114,105,102,121,95,116,111,107,101,110]

samples = [(B.pack [104,117,98,46,109,111,100,101], B.empty)
          ,(B.pack [104,117,98,46,116,111,112,105,99], B.empty)
          ,(B.pack [104,117,98,46,99,97,108,108,98,97,99,107], B.empty)]
          
-- Param parser process.
process = printer <+< buildRequest <+< parseParam <+< filterParam <+< sourceMaybe samples -- hub.topic