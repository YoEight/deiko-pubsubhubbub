{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, FlexibleContexts #-}

module Subscription.Verification (verification) where

import Subscription.Params

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Error.Class
import Control.Monad.Trans.Either
import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Pipe

import Data.Bson
import Data.Either
import Data.Monoid
import Data.Text.Encoding

import Database.MongoDB hiding (Pipe(..))

import Subscription.Conf

data Response = Success | Failure | Pending

verification :: (MonadIO m, MonadReader Conf m, MonadError e m)
                => Pipe (Either String Req) Response m ()
verification = await >>= go
  where
    go (Left _)    = yield Failure
    go (Right req)
      | isSubscription req = if isSyncMode req then sync req else async req
      | otherwise          = async req

sync :: (MonadIO m, MonadReader Conf m, MonadError e m)
        => Req
        -> Pipe a Response m ()
sync req = do
  result <- (lift . runEitherT) $ (confirmation req) >> (saveSubscription req)
  either (yield . const Failure) (yield . const Success) result 
  
async :: (MonadIO m, MonadReader Conf m, MonadError e m) =>
         Req
         -> Pipe a Response m ()
async req = do
  result <- lift $ (runEitherT . saveSubscription) req
  either (yield . const Failure) (yield . const Success) result

-- Database call. Will use MongoDB
saveSubscription :: (MonadIO m, MonadReader Conf m, MonadError e m)
                    => Req
                    -> EitherT () m ()
saveSubscription req = EitherT go
  where
    go = runDbAction (return . Left . const ()) (fmap Right (createSubscription req))

-- Do a GET request using action request callback
confirmation :: MonadIO m
                => Req
                -> EitherT () m ()
confirmation = error "todo"

createSubscription :: Req -> Action IO ()
createSubscription (Req (Callback callback) _ (Topic topic) _ optionals) =
  let start = ["callback" =: (decodeUtf8 callback)
              ,"topic"    =: (decodeUtf8 topic)]
      go (LeaseSeconds n) = merge ["lease_seconds" =: n]
      go (Secret secret)  = merge ["secret" =: secret]

      document = foldr go start optionals
  in insert_ "hub_subscription" document

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