{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Web.Deiko.Hub.Persist where

import           Web.Deiko.Hub.Parse  (validateUrl)
import           Web.Deiko.Hub.Types

import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.Trans

import           Data.Binary          hiding (get)
import           Data.Binary.Put
import qualified Data.ByteString      as B
import           Data.ByteString.Lazy (fromStrict, toStrict)
import           Data.Foldable
import           Data.Hashable
import           Data.Monoid
import           Data.String
import qualified Data.Text            as S
import           Data.Text.Encoding   (encodeUtf8)
import           Data.Traversable

import           Database.Redis       hiding (decode)
import           Database.SQLite      hiding (Status)

import           System.Directory

sqliteDbName :: String
sqliteDbName = "/hub.db"

executeRedis :: (MonadIO m, MonadError HubError m) => Redis (Either Reply a) -> m a
executeRedis action = do
  result <- liftIO $ do connection <- connect defaultConnectInfo
                        runRedis connection action
  either (throwError . InternalError . fromString . show) return result

saveSubscription :: (Verification v, ToValue v, RedisCtx m f) => Sub v -> m (f Integer)
saveSubscription sub =
    rpush (B.append "sub:" (toStrict $ runPut $ put $ hash sub)) [toByteString sub]

loadSubscriptionHistory :: RedisCtx m f
                        => B.ByteString
                        -> B.ByteString
                        -> m (f [B.ByteString])
loadSubscriptionHistory callback topic =
    lrange (B.append "sub:" $ toStrict $ runPut $ put $ hash (callback, topic)) 0 0

pushPublishEvent :: RedisCtx m f => Pub Submitted -> m (f Integer)
pushPublishEvent pub = error "todo" -- rpush (B.append "publish:events:" url) [toByteString e]

pushAsyncSubRequest :: RedisCtx m f => SubParams -> m (f Integer)
pushAsyncSubRequest = rpush "sub:queue" . return . toByteString

popAsyncSubRequest :: (RedisCtx m f, Applicative m, Traversable f) => m (f (Maybe SubParams))
popAsyncSubRequest = lpop "sub:request" >>= (traverse (traverse fromByteString))

pushPublishQueue :: RedisCtx m f => Pub Submitted -> m (f Integer)
pushPublishQueue (Pub _ url) = rpush "publish:queue" [encodeUtf8 url]

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
                       -> Sub (Deletion Verified)
                       -> m ()
unregisterSubscription handle (Sub _ (SubInfos _ (SubParams callback _ topic _ _ _ _) _)) =
    do result <- liftIO $ execParamStatement_ handle query params
       maybe (return ()) (throwError . InternalError . fromString) result
           where
             query  = "delete from subscriptions where topic = :topic and callback = :callback"
             params = [(":topic", Blob $ encodeUtf8 topic)
                      ,(":callback", Blob $ encodeUtf8 callback)]

registerSubscription :: (MonadIO m, MonadError HubError m)
                     => SQLiteHandle
                     -> Sub Verified
                     -> m ()
registerSubscription handle (Sub _ (SubInfos _ (SubParams callback _ topic _ leaseSec secr _) date)) =
    do result <- liftIO $ execParamStatement_ handle query params
       maybe (return ()) (throwError . InternalError . fromString) result
           where
             query  = "insert into subscriptions (topic, callback, lease_second, secret) values (:topic, :callback, :lease, :secret)"
             secret = maybe Null (Blob . encodeUtf8) secr
             lease  = maybe Null (Int . fromIntegral) leaseSec
             params = [(":topic", Blob $ encodeUtf8 topic)
                      ,(":callback", Blob $ encodeUtf8 callback)
                      ,(":lease", lease)
                      ,(":secret", secret)]

registerFeed :: RedisCtx m f => B.ByteString -> m (f Status)
registerFeed topic = set (B.append "known_feed:" topic) "1"

knownFeed :: RedisCtx m f => B.ByteString -> m (f Bool)
knownFeed topic = exists (B.append "known_feed:"  topic)

initSubscriberCounter :: RedisCtx m f => B.ByteString -> m (f Status)
initSubscriberCounter topic = set (B.append "subs:" topic) "0"

incrSubscriberCount :: RedisCtx m f => B.ByteString -> m (f Integer)
incrSubscriberCount topic = incr (B.append "subs:" topic)

subscriberCount :: (RedisCtx m f, Functor f) => B.ByteString -> m (f (Maybe Integer))
subscriberCount topic = liftM (((decode . fromStrict) <$>) <$>) (get $ B.append "subs:" topic)

validatePublishRequest :: S.Text -> Redis (Either Reply (Maybe (Pub Submitted)))
validatePublishRequest url = maybe checks (const nothing) (validateUrl url)
    where
      encodedUrl = encodeUtf8 url

      checks = bindRedis (\exist -> if exist then bindRedis checkCount $ subscriberCount encodedUrl else nothing) $
               knownFeed encodedUrl

      checkCount (Just count)
          | count > 0 = just (Pub Submitted url)
          | otherwise = nothing
      checkCount _ = error "Impossible situation in validationPublishRequest"

      nothing = returnRedis Nothing
      just    = returnRedis . Just

returnRedis :: a -> Redis (Either Reply a)
returnRedis = return . Right

mapRedis :: (a -> b) -> Redis (Either Reply a) -> Redis (Either Reply b)
mapRedis f = fmap (fmap f)

bindRedis :: (a -> Redis (Either Reply b)) -> Redis (Either Reply a) -> Redis (Either Reply b)
bindRedis f m = (either (return . Left . id) f) =<< m

-- What a shame this isn't in stlib
instance Foldable (Either a) where
    foldMap f (Right a) = f a
    foldMap _ _         = mempty

instance Traversable (Either a) where
    traverse f (Right b) = Right <$> (f b)
    traverse _ (Left a)  = pure (Left a)
