module Subscription where

import Subscription.Conf
import Subscription.Parse
import Subscription.Verification

import Data.ByteString
import Data.Foldable

import Control.Monad.Reader
import Control.Pipe

sourceMaybe :: (Monad m, Foldable f)
               => f a
               -> Pipe () (Maybe a) m ()
sourceMaybe = (>> yield Nothing) . traverse_ (yield . Just) 

subscription :: [(ByteString, Maybe ByteString)]
                -> Pipe () Response (ReaderT Conf IO) ()
subscription xs = verification <+< parseRequest <+< sourceMaybe xs