{-# LANGUAGE FlexibleContexts #-}

module Subscription where

import Subscription.Conf
import Subscription.Parse
import Subscription.Verification

import Data.ByteString
import Data.Foldable

import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.Error.Class
import Control.Monad.Trans
import Control.Pipe

sourceMaybe :: (Monad m, Foldable f)
               => f a
               -> Pipe () (Maybe a) m r
sourceMaybe = forever . (>> yield Nothing) . traverse_ (yield . Just) 

subscription :: (MonadIO m, MonadReader Conf m, MonadError e m)
                => [(ByteString, Maybe ByteString)]
                -> Pipe () Response m r
subscription xs = verification <+< parseRequest <+< sourceMaybe xs