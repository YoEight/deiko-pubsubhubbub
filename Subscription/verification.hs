{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, FlexibleContexts #-}

module Subscription.Verification (verification, Response(..)) where

import Subscription.Params

import Control.Monad
import Control.Monad.Random
import Control.Monad.Trans
import Control.Monad.Trans.Class
import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Either
import Control.Pipe

import Data.Bson
import Data.Char
import Data.List
import Data.Either
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import Data.Text.Encoding

import Database.MongoDB hiding (Pipe(..), find)

import Network.Curl

import Subscription.Conf

data Response = Success | Failure | Pending deriving Show

verification :: (MonadIO m, MonadReader Conf m, MonadError e m)
                => Pipe (Either String Req) Response m r
verification = forever $ await >>= go
  where
    go (Left _)    = yield Failure
    go (Right req)
      | isSubscription req = if isSyncMode req then sync req else async req
      | otherwise          = async req

sync :: (MonadIO m, MonadReader Conf m, MonadError e m)
        => Req
        -> Pipe a Response m r
sync req = forever $ do
  result <- (lift . runEitherT) $ (confirmation req) >> (saveSub req True)
  either (yield . const Failure) (yield . const Success) result 
  
async :: (MonadIO m, MonadReader Conf m, MonadError e m)
         => Req
         -> Pipe a Response m r
async req = forever $ do
  result <- lift $ runEitherT $ saveSub req False
  either (yield . const Failure) (yield . const Success) result

-- Database call. Will use MongoDB
saveSub :: (MonadIO m, MonadReader Conf m, MonadError e m)
           => Req
           -> Bool
           -> EitherT () m ()
saveSub req@(Req (Callback callback) _ (Topic topic) _ _) verified = EitherT go
  where
    go = let cText = decodeUtf8 callback
             tText = decodeUtf8 topic
             doc   = createSubDoc req
             action
               | verified  = findSub cText tText >>= save "hub_subscription" . maybe doc (merge doc . include ["_id"])
               | otherwise = createTempSub doc
               
         in runDbAction (return . Left . const ()) (fmap Right action)

-- Do a GET request using action request callback
confirmation :: MonadIO m
                => Req
                -> EitherT () m ()
confirmation req = EitherT go
  where
    go = liftIO $ withCurlDo $ do
      (challenge, url) <- confirmationRequest req
      response <- curlGetResponse_ url [] :: IO (CurlResponse_ [(String, String)] String)
      case respStatus response of
        404    -> return $ Left () -- subscriber doesn't agree with the action
        status | 200 >= status && 300 < status && validateSubcriberResponse challenge (respBody response) -> return $ Right ()
               | otherwise ->  return $ Left ()

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

createTempSub :: Document -> Action IO ()
createTempSub = insert_ "hub_temp_subscription"

createSubDoc :: Req -> Document
createSubDoc (Req (Callback callback) _ (Topic topic) _ optionals) =
  let start = ["callback"  =: (decodeUtf8 callback)
              ,"topic"     =: (decodeUtf8 topic)]
              
      go (LeaseSeconds n) = merge ["lease_seconds" =: n]
      go (Secret secret)  = merge ["secret"        =: secret]

  in foldr go start optionals

findSub :: T.Text
           -> T.Text
           -> Action IO (Maybe Document)
findSub callback topic =
  let query = ["callback" =: callback
              ,"topic"    =: topic]
  in findOne (select query "hub_subscription")

runDbAction :: (MonadIO m, MonadError e m, MonadReader Conf m)
               => (e -> m a)
               -> Action IO a
               -> m a
runDbAction handle action = do
  conf <- asks confDb
  let run = do
        p <- runIOE $ connect $ dbHost conf
        r <- access p master (dbCollection conf) action
        close p
        either (error . show) return r
  (liftIO run) `catchError` handle