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
import qualified Data.Text as T

source :: (Monad m, Foldable f) =>
          f a
          -> Pipe () a m ()
source = traverse_ yield

stripPrefix :: (Monad m, Eq a, ListLike f a) =>
               f ->
               Pipe f f m r
stripPrefix prefix = forever $ go prefix =<< await
  where
    go prefix value
      | null value  = return ()
      | null prefix = yield value
      | otherwise = let (px, vx) = (head prefix, head value)
                    in if px == vx then go (tail prefix) (tail value) else return ()

parseParam :: Monad m => Pipe (B.ByteString, B.ByteString) SubParam  m ()
parseParam = forever $ await >>= uncurry select

-- Yields a SubParam according to param name. Also performs
-- basic validation on param value
select :: Monad m =>
          B.ByteString
          -> B.ByteString
          -> Pipe a SubParam m () 
select name value
  | name == hub_callback      = yield $ Callback T.empty
  | name == hub_mode          = yield $ Mode T.empty
  | name == hub_topic         = yield $ Topic T.empty
  | name == hub_lease_seconds = yield $ LeaseSeconds 0
  | name == hub_secret        = yield $ Secret T.empty
  | name == hub_verify_token  = yield $ VerifyToken T.empty
  | name == hub_verify        = yield $ Verify Sync
  | otherwise                 = return ()

-- Discard param request that doesn't start by 'hub.'
hubPrefix :: Monad m => Pipe B.ByteString B.ByteString m ()
hubPrefix = stripPrefix hub_prefix

printer = forever ((lift . print) =<< await)

-- Simulate Arrow.first 
first :: (Monad m, r ~ ()) =>
         Pipe a b m r
         -> Pipe (a, c) (b, c) m r
first inner = ((\(a, c) -> ((yield . (, c)) =<< await) <+< inner <+< yield a) =<< await)

hub_prefix = B.pack [104,117,98,46]

hub_callback = B.pack [99,97,108,108,98,97,99,107]

hub_mode = B.pack [109,111,100,101]

hub_topic = B.pack [116,111,112,105,99]

hub_verify = B.pack [118,101,114,105,102,121]

hub_lease_seconds = B.pack [108,101,97,115,101,95,115,101,99,111,110,100,115]

hub_secret = B.pack [115,101,99,114,101,116]

hub_verify_token = B.pack [118,101,114,105,102,121,95,116,111,107,101,110]

-- Param parser process.
process = printer <+< parseParam <+< first hubPrefix <+< source [(B.pack [104,117,98,46,116,111,112,105,99], B.empty)] -- hub.topic