{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Web.Deiko.Hub.Persist where

import           Web.Deiko.Hub.Types

import           Control.Monad.Error
import           Control.Monad.Trans

import qualified Data.ByteString     as B
import           Data.String

import           Database.Redis
import           Database.SQLite

import           System.Directory

sqliteDbName :: String
sqliteDbName = "/hub.db"

executeRedis :: (MonadIO m, MonadError HubError m) => Redis (Either Reply a) -> m a
executeRedis action = do
  result <- liftIO $ do connection <- connect defaultConnectInfo
                        runRedis connection action
  either (throwError . InternalError . fromString . show) return result

saveHubEvent :: RedisCtx m f
             => B.ByteString
             -> B.ByteString
             -> HubEvent
             -> m (f Integer)
saveHubEvent callback topic event =
    rpush (B.append "events:" $ B.append callback $ B.append ":" topic) [toByteString event]

loadEvents :: RedisCtx m f
           => B.ByteString
           -> B.ByteString
           -> m (f [B.ByteString])
loadEvents callback topic =
    lrange (B.append "events:" $ B.append callback $ B.append ":" topic) 0 0

pushPublishEvent :: RedisCtx m f
                 => B.ByteString
                 -> HubEvent
                 -> m (f Integer)
pushPublishEvent url e = rpush (B.append "publish:events:" url) [toByteString e]

pushPublishQueue :: RedisCtx m f => B.ByteString -> m (f Integer)
pushPublishQueue  = rpush "publish:queue" . return

withSqliteConnection :: (MonadIO m, MonadError HubError m)
                     => (SQLiteHandle -> m a)
                     -> m a
withSqliteConnection f =
    do handle <- liftIO $ getCurrentDirectory >>= \dir -> openConnection (dir ++ sqliteDbName)
       a      <- f handle
       liftIO $ closeConnection handle
       return a

unregisterSubscription :: (MonadIO m, MonadError HubError m)
                       => SQLiteHandle
                       -> B.ByteString
                       -> B.ByteString
                       -> m ()
unregisterSubscription handle topic callback =
    do result <- liftIO $ execParamStatement_ handle query params
       maybe (return ()) (throwError . InternalError . fromString) result
           where
             query  = "delete from subscriptions where topic_id = :topic_id and callback_id = :callback_id"
             params = [(":topic_id", Blob topic)
                      ,(":callback_id", Blob callback)]

registerSubscription :: (MonadIO m, MonadError HubError m)
                     => SQLiteHandle
                     -> B.ByteString
                     -> B.ByteString
                     -> m ()
registerSubscription handle topic callback =
    do result <- liftIO $ execParamStatement_ handle query params
       maybe (return ()) (throwError . InternalError . fromString) result
           where
             query  = "insert into subscriptions (topic_id, callback_id) values (:topic_id, :callback_id)"
             params = [(":topic_id", Blob topic)
                      ,(":callback_id", Blob callback)]
