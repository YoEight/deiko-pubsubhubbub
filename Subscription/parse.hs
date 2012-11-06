module Subscription.Parse where

import Subscription.Params

import Control.Pipe
import qualified Data.ByteString as B
import qualified Data.Text as T

sourceParams :: Monad m =>
                [(B.ByteString, B.ByteString)]
                -> Pipe () (B.ByteString, B.ByteString) m ()
sourceParams []     = return ()
sourceParams (x:xs) = yield x >> sourceParams xs

parseParam :: Monad m =>
              Pipe (B.ByteString, B.ByteString) SubParam  m ()
parseParam = error "todo"

hubPrefix :: Monad m =>
             Pipe B.ByteString B.ByteString m ()
hubPrefix = (yield . snd . B.breakSubstring (B.pack [104,117,98,46])) =<< await