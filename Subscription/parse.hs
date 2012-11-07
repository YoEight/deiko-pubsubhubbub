{-# LANGUAGE NoMonomorphismRestriction #-}

module Subscription.Parse where

import Subscription.Params

import Control.Monad
import Control.Monad.Trans.Class
import Control.Pipe

import qualified Data.ByteString as B
import Data.Foldable
import qualified Data.Text as T

source :: (Monad m, Foldable f) =>
          f a
          -> Pipe () a m ()
source = traverse_ yield

parseParam :: Monad m => Pipe (B.ByteString, B.ByteString) SubParam  m ()
parseParam = error "todo"

hubPrefix :: Monad m => Pipe B.ByteString Bool m ()
hubPrefix = (yield . B.isPrefixOf (B.pack [104,117,98,46])) =<< await

printer = forever ((lift . print) =<< await)

test = runPipe (printer <+< hubPrefix <+< source [(B.pack [104,117,98,46,116,111,112,105,99])]) -- hub.topic