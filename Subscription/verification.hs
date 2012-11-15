{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, FlexibleContexts #-}

module Subscription.Verification (verification) where

import Subscription.Params

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Class
import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Either
import Control.Pipe

import Data.Bson
import Data.Either
import qualified Data.Text as T
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
  result <- (lift . runEitherT) $ (confirmation req) >> (saveSub req True)
  either (yield . const Failure) (yield . const Success) result 
  
async :: (MonadIO m, MonadReader Conf m, MonadError e m)
         => Req
         -> Pipe a Response m ()
async req = do
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
confirmation = error "todo"

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