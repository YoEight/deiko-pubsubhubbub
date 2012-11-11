module Subscription.Test where

import Subscription.Parse

import Control.Monad
import Control.Monad.Trans.Class
import Control.Pipe

import Data.Foldable
import qualified Data.ByteString as B
import Data.ByteString.Char8

sourceMaybe :: (Monad m, Foldable f) =>
               f a
               -> Pipe () (Maybe a) m ()
sourceMaybe fa = traverse_ (yield . Just) fa >> yield Nothing

printer :: Show a =>
           Pipe a C IO ()
printer = forever $ (lift . print) =<< await

samples :: [(B.ByteString, B.ByteString)]
samples = [(pack "hub.topic", pack "http://hackage.haskell.org/packages/archive/text/0.11.2.3/doc/html/Data-Text-Encoding.html")
          ,(pack "hub.callback", pack "https://github.com/Frege/frege")
          ,(pack "hub.verify", pack "sync")
          ,(pack "hub.mode", pack "subscribe")
          ,(pack "hub.secret", pack "azerty")
          ,(pack "hub.verify_token", pack "echo")
          ,(pack "hub.lease_seconds", pack "42")]

test_parseRequest :: Pipe () C IO ()
test_parseRequest = printer <+< parseRequest <+< sourceMaybe samples
          