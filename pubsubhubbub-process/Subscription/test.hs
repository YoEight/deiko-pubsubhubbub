{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}

module Subscription.Test where

import Subscription.Conf
import Subscription.Parse
import Subscription.Persistence
import Subscription.Params

import Control.Monad.Error.Class
import Control.Monad.Trans
import Control.Monad.RWS.Lazy

import qualified Data.ByteString as B
import Data.Machine

import Database.MongoDB

conf :: Conf 
conf = Conf (DB (Host "127.0.0.1" defaultPort) "hub_deiko")

report_debug :: Report
report_debug = Report Nothing Sync

printer :: (Show a, MonadIO m)
           => ProcessT m a ()
printer = repeatedly $ (liftIO . print) =<< await

samples :: [(B.ByteString, Maybe B.ByteString)]
samples = [("hub.topic",  Just "http://hackage.haskell.org/packages/archive/text/0.11.2.3/doc/html/Data-Text-Encoding.html")
          ,("hub.callback", Just "https://github.com/Frege/frege")
          ,("hub.verify", Just "sync")
          ,("hub.mode", Just "subscribe")
          ,("hub.secret", Just "azerty")
          ,("hub.verify_token", Just "echo")
          ,("hub.lease_seconds", Just "42")]

test_parse :: MonadIO m => ProcessT (RWST () () Report m) () ()
test_parse = printer <~ parseRequest <~ source samples

test_mongodb_sub :: (MonadIO m, MonadError e m) => ProcessT (RWST Conf () Report m) () ()
test_mongodb_sub = persist <~ parseRequest <~ source samples