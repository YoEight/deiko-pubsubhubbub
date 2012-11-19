{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Subscription.Persistence (persist) where

import Subscription.Params
import Subscription.Conf
import Subscription.Util

import Control.Monad.Trans
import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class

import Data.Machine
import qualified Data.Text as T
import Data.Text.Encoding

import Data.Bson
import Database.MongoDB

persist :: (MonadState Report m, MonadReader Conf m, MonadError e m, MonadIO m)
           => ProcessT m Req a
persist = construct $ await >>= go
  where
    go req
      | isSubscription req = let doc = createSubDoc req
                             in do
                               strategy <- getCurrentStrategy
                               case strategy of
                                 Sync  -> saveSub req
                                 Async -> runDbAction (const stop) (createTempSub doc)

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

    
-- Database call. Will use MongoDB
saveSub :: (MonadIO m, MonadReader Conf m, MonadError e m)
           => Req
           -> PlanT k o m ()
saveSub req@(Req (Callback callback) _ (Topic topic) _ _) = go
  where
    go = let cText = decodeUtf8 callback
             tText = decodeUtf8 topic
             doc   = createSubDoc req
             action = findSub cText tText >>= save "hub_subscription" . maybe doc (merge doc . include ["_id"])
               
         in runDbAction (const stop) action
