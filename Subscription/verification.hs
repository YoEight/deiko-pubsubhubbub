{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, FlexibleContexts #-}

module Subscription.Verification (verification) where

import Subscription.Params
import Subscription.Conf
import Subscription.Util

import Control.Monad
import Control.Monad.Random
import Control.Monad.Trans
import Control.Monad.State.Class

import Data.Char
import Data.List
import qualified Data.ByteString.Char8 as C
import Data.Machine
import qualified Data.Text as T
import Data.Text.Encoding

import Network.Curl

verification :: (MonadState Report m, MonadIO m)
                => ProcessT m Req Req
verification = construct $ await >>= go
  where
    go req
      | isSyncMode req = sync req >> yield req
      | otherwise      = changeStrategy Async >> yield req

-- Do a GET request using action request callback
sync :: (MonadState Report m, MonadIO m)
        => Req
        -> PlanT k o m ()
sync req = let result = withCurlDo $ do
                 (challenge, url) <- confirmationRequest req
                 response         <- curlGetResponse_ url [] :: IO (CurlResponse_ [(String, String)] String)
                 return (challenge, response)
           in do
             (challenge, response) <- liftIO result
             case respStatus response of
               404 -> reportError ClientSide "unreachable callback url"
               status | 200 >= status && status < 300 && validateSubcriberResponse challenge (respBody response) -> return ()
                      | otherwise -> reportError ClientSide "error during confirmation"

confirmationRequest :: Req -> IO (String, String)
confirmationRequest (Req (Callback callback) (Mode mode) (Topic topic) _ optionals) =
  let modeStr Subscribe   = "&hub.mode=subscribe"
      modeStr Unsubscribe = "&hub.mode=unsubscribe"
      topicStr = "&hub.topic=" ++ (C.unpack topic)
      
      go (LeaseSeconds n)    = "&hub.lease_seconds=" ++ (show n)
      go (VerifyToken token) = "&hub.verify_token=" ++ (T.unpack token)

      params = foldr ((++) . go) (topicStr ++ (modeStr mode)) optionals
  in do
    challenge <- randomString
    return $ (challenge, (C.unpack callback) ++ params ++ "hub.challenge=" ++ challenge)

validateSubcriberResponse :: String -> String -> Bool
validateSubcriberResponse upstream = maybe False (== upstream) . stripPrefix "hub.challenge=" 

randomString :: IO String
randomString = do
  values <- evalRandIO (sequence $ replicate 10 rnd)
  return $ map chr values
    where
      rnd :: RandomGen g => Rand g Int
      rnd = getRandomR (65, 90) -- A-Za-z
