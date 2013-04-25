{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Web.Deiko.Hub.Persist where

import           Web.Deiko.Hub.Types

import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.Trans

import           Data.Binary
import           Data.Binary.Put
import qualified Data.ByteString      as B
import           Data.ByteString.Lazy (toStrict)
import           Data.Foldable
import           Data.Hashable
import           Data.Monoid
import           Data.String
import           Data.Traversable

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

saveSubscription :: RedisCtx m f => Subscription -> m (f Integer)
saveSubscription sub =
    rpush (B.append "subscription:" (toStrict $ runPut $ put $ hash sub)) [toByteString sub]

loadSubscriptionHistory :: RedisCtx m f
                        => B.ByteString
                        -> B.ByteString
                        -> m (f [B.ByteString])
loadSubscriptionHistory callback topic =
    lrange (B.append "subscription:" $ toStrict $ runPut $ put $ hash (callback, topic)) 0 0

pushPublishEvent :: RedisCtx m f
                 => B.ByteString
                 -> HubEvent
                 -> m (f Integer)
pushPublishEvent url e = error "todo" -- rpush (B.append "publish:events:" url) [toByteString e]

pushAsyncSubRequest :: RedisCtx m f => SubParams -> m (f Integer)
pushAsyncSubRequest = rpush "subscription:queue" . return . toByteString

popAsyncSubRequest :: (RedisCtx m f, Applicative m, Traversable f) => m (f (Maybe SubParams))
popAsyncSubRequest = lpop "subscription:request" >>= (traverse (traverse fromByteString))

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

-- What a shame this isn't in stlib
instance Foldable (Either a) where
    foldMap f (Right a) = f a
    foldMap _ _         = mempty

instance Traversable (Either a) where
    traverse f (Right b) = Right <$> (f b)
    traverse _ (Left a)  = pure (Left a)
