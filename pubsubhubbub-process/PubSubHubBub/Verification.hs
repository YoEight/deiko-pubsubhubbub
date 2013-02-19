{-# LANGUAGE OverloadedStrings #-}
module PubSubHubBub.Verification where

import           Control.Monad.State

import qualified Data.ByteString            as B
import           Data.Char
import           Data.Machine
import qualified Data.Map                   as M
import           Data.Maybe

import           Control.Monad.Random
import           Control.Monad.Random.Class
import           Control.Monad.Trans
import           Control.Monad.Trans.Either

import           PubSubHubBub.Types

import           System.Random

subReqVerif :: SubReq -> Machine (EitherT String IO) SubReq
subReqVerif req = error "todo"

insertOptional :: (a -> Maybe ParamLit)
               -> B.ByteString
               -> a
               -> M.Map B.ByteString B.ByteString
               -> M.Map B.ByteString B.ByteString
insertOptional k key t map = maybe map go (k t)
  where
    go (PBytes xs) = M.insert key xs map
    go (PInt i)    = M.insert key (packString (show i)) map

insertLit :: (a -> ParamLit)
          -> B.ByteString
          -> a
          -> M.Map B.ByteString B.ByteString
          -> M.Map B.ByteString B.ByteString
insertLit k key t = M.insert key (go $ k t)
  where
    go (PBytes xs) = xs
    go (PInt i)    = packString (show i)

confirmationRequestParams :: SubReq -> IO (M.Map B.ByteString B.ByteString)
confirmationRequestParams req = execStateT go M.empty
  where
    go :: StateT (M.Map B.ByteString B.ByteString) IO ()
    go = do
      challenge <- lift $ randomString
      modify (insertLit subReqMode hub_mode req)
      modify (insertLit subReqTopic hub_topic req)
      modify (insertOptional subReqLeaseSeconds hub_lease_seconds req)
      modify (insertOptional subReqVerifyToken hub_verify_token req)
      modify (M.insert hub_challenge (packString challenge))

randomString :: IO String
randomString = do
  values <- evalRandIO (sequence $ replicate 10 rnd)
  return $ map chr values
    where
      rnd :: RandomGen g => Rand g Int
      rnd = getRandomR (65, 90) -- A-Za-z

-- This is very unfortunate. But not all dependencies have migrated to bytestring 0.10.2.*
packString :: String -> B.ByteString
packString = B.pack . fmap (fromInteger . toInteger . ord)
