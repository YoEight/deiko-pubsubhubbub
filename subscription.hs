{-# LANGUAGE FlexibleContexts #-}

module Subscription where

import Subscription.Conf
import Subscription.Parse
import Subscription.Params
import Subscription.Verification
import Subscription.Persistence

import Data.ByteString
import Data.Machine

import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.Error.Class
import Control.Monad.State.Class
import Control.Monad.Trans

subscription :: (MonadIO m, MonadState Report m, MonadReader Conf m, MonadError e m)
                => ProcessT m (ByteString, Maybe ByteString) a
subscription  = persist <~ verification <~ parseRequest