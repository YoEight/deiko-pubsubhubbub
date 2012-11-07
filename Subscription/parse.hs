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
parseParam = forever $ await >>= go
  where
    go (param, value) = (select value =<< await) <+< hubPrefix <+< yield param

-- Yields a SubParam according to param name. Also performs
-- basic validation on param value
select :: Monad m =>
          B.ByteString
          -> B.ByteString
          -> Pipe B.ByteString SubParam m () 
select value name = error "todo"

-- Discard param request that doesn't start by 'hub.'
hubPrefix :: Monad m => Pipe B.ByteString B.ByteString m ()
hubPrefix = stripPrefix (B.pack [104,117,98,46])

printer = forever ((lift . print) =<< await)

-- Simulate Arrow.first 
first :: (Monad m, r ~ ()) =>
         Pipe a b m r
         -> Pipe (a, c) (b, c) m r
first inner = ((\(a, c) -> ((yield . (,c)) =<< await) <+< inner <+< yield a) =<< await)
  
-- Param parser process.
process = printer <+< parseParam <+< first hubPrefix <+< source [(B.pack [104,117,98,46,116,111,112,105,99], B.empty)] -- hub.topic