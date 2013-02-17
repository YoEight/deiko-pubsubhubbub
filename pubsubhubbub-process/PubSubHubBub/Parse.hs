{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module PubSubHubBub.Parse where

import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans

import           Data.Bifunctor
import qualified Data.ByteString                          as B
import           Data.Either
import           Data.Foldable
import           Data.Machine
import qualified Data.Map                                 as M

import           PubSubHubBub.Types

parseLiteral :: Process (B.ByteString, B.ByteString) (B.ByteString, ParamLit)
parseLiteral = repeatedly (await >>= go)
  where
    go (key, value) =
      either (const $ yield (key, PBytes value)) (yield . (key,)) (PInt <$> parseInt value)

parseParams :: Process (B.ByteString, B.ByteString) [(B.ByteString, ParamLit)]
parseParams = (repeatedly $ go []) <~ parseLiteral
  where
    go xs = do
      x <- await <|> yield (reverse xs) *> empty
      go (x:xs)

parseSubReq :: Process [(B.ByteString, ParamLit)] (Either String SubReq)
parseSubReq = repeatedly (await >>= go)
  where
    go [] = empty
    go xs =
      let params     = execState (traverse_ makeMap xs) M.empty
          validation = runReaderT validateSubReqParams params
      in yield validation *> empty
    makeMap (key, lit) = modify (step key lit)
    step key lit map
      | key == hub_callback      = M.insertWith const "hub_callback" lit map
      | key == hub_mode          = M.insertWith const "hub_mode" lit map
      | key == hub_verify        = M.insertWith merge "hub_verify" lit map
      | key == hub_lease_seconds = M.insertWith const "hub_lease_seconds" lit map
      | key == hub_secret        = M.insertWith const "hub_secret" lit map
      | key == hub_verify_token  = M.insertWith const "hub_verify_token" lit map
    merge n (PList xs) = PList (xs ++ [n])
    merge n o          = PList [o, n]

hub_callback :: B.ByteString
hub_callback = "hub.callback"

hub_mode :: B.ByteString
hub_mode = "hub.mode"

hub_topic :: B.ByteString
hub_topic = "hub.topic"

hub_verify :: B.ByteString
hub_verify = "hub.verify"

hub_lease_seconds :: B.ByteString
hub_lease_seconds = "hub.lease_seconds"

hub_secret :: B.ByteString
hub_secret = "hub.secret"

hub_verify_token :: B.ByteString
hub_verify_token = "hub.verify_token"

hub_url :: B.ByteString
hub_url = "hub.url"

hub_challenge :: B.ByteString
hub_challenge = "hub.challenge"
