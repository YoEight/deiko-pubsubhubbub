{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Web.Deiko.Hub.Persist where

import           Web.Deiko.Hub.Parse   (validateUrl)
import           Web.Deiko.Hub.Types

import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.Trans

import           Data.Binary           hiding (get)
import           Data.Binary.Put
import qualified Data.ByteString       as B
import           Data.ByteString.Lazy  (fromStrict, toStrict)
import           Data.Foldable
import           Data.Functor.Identity (Identity (..))
import           Data.Hashable
import           Data.Monoid
import           Data.String
import qualified Data.Text             as S
import           Data.Text.Encoding    (encodeUtf8)
import           Data.Time.Clock       (getCurrentTime)
import           Data.Traversable

import           Database.Redis        hiding (decode)
import           Database.SQLite       hiding (Status)

import           System.Directory

sqliteDbName :: String
sqliteDbName = "/hub.db"

executeRedis :: (MonadIO m, MonadError HubError m)
             => Redis (Either Reply a)
             -> m a
executeRedis action = do
  result <- liftIO $
            connect defaultConnectInfo >>=
                        (flip runRedis $ action)
  either (throwError . InternalError . fromString . show) return result

saveSubscription :: (Verification v, ToValue v, RedisCtx m f)
                 => Sub v
                 -> m (f Integer)
saveSubscription sub =
    rpush (B.append "sub:" (toStrict $ runPut $ put $ hash sub))
            [toByteString sub]

loadSubscriptionHistory :: RedisCtx m f
                        => B.ByteString
                        -> B.ByteString
                        -> m (f [B.ByteString])
loadSubscriptionHistory callback topic =
  let key = (B.append "sub:" $ toStrict $ runPut $ put $ hash (callback, topic))
  in lrange key 0 0

pushPublishEvent :: (RedisCtx m f, Publishing p, ToValue p)
                 => Pub p
                 -> m (f Integer)
pushPublishEvent e@(Pub _ (PubInfos url _ _)) =
  rpush (B.append "publish:events:" (encodeUtf8 url)) [toByteString e]

pushAsyncSubRequest :: (Pending v, RedisCtx m f)
                    => Sub v
                    -> m (f Integer)
pushAsyncSubRequest =
  rpush "sub:queue" . return . toByteString . subParams . subInfos

type BothPending = Either (Sub NotVerified) (Sub (Deletion NotVerified))

popAsyncSubRequest :: (RedisCtx m f, Applicative m, Traversable f)
                   => m (f (Maybe BothPending))
popAsyncSubRequest = lpop "sub:request" >>= (traverse (traverse go))
  where
    go bytes =
      let sub   = fromByteString bytes
          unsub = runIdentity $ fromByteString bytes
      in pure $ maybe (Right unsub) Left sub

pushPublishQueue :: RedisCtx m f => Pub Submitted -> m (f Integer)
pushPublishQueue (Pub _ (PubInfos url _ _)) =
  rpush "publish:queue" [encodeUtf8 url]

withSqliteConnection :: (MonadIO m, MonadError HubError m)
                     => (SQLiteHandle -> m a)
                     -> m a
withSqliteConnection f =
  do handle <- liftIO $ getCurrentDirectory >>=
               \dir -> openConnection (dir ++ sqliteDbName)
     a      <- f handle
     liftIO $ closeConnection handle
     return a

unregisterSub :: (MonadIO m, MonadError HubError m, Ended v w)
              => Sub w
              -> SQLiteHandle
              -> m ()
unregisterSub sub handle =
  do result <- liftIO $ execParamStatement_ handle
               deleteSubQuery $
               deleteSubParams (subParams $ subInfos sub)
     maybe (return ()) (throwError . InternalError . fromString) result

deleteSubQuery :: String
deleteSubQuery =
  "delete from subscriptions where topic = :topic and callback = :callback"

deleteSubParams :: SubParams -> [(String, Value)]
deleteSubParams (SubParams cb _ topic _ _ _ _) =
  [(":topic", Blob $ encodeUtf8 topic)
  ,(":callback", Blob $ encodeUtf8 cb)]

registerSub :: (MonadIO m, MonadError HubError m, Ended v w)
            => Sub w
            -> SQLiteHandle
            -> m ()
registerSub sub handle =
  do result <- liftIO $ execParamStatement_ handle
               registerSubQuery $
               registerSubParams $ subParams $ subInfos sub
     maybe (return ()) (throwError . InternalError . fromString) result

registerSubQuery :: String
registerSubQuery =
  "insert into subscriptions (topic, callback, lease_second, secret) values " ++
  "(:topic, :callback, :lease, :secret)"

registerSubParams :: SubParams -> [(String, Value)]
registerSubParams (SubParams cb _ topic _ lease secr _) =
  [(":topic", Blob $ encodeUtf8 topic)
  ,(":callback", Blob $ encodeUtf8 cb)
  ,(":lease", lease0)
  ,(":secret", secret)]
  where
    secret = maybe Null (Blob . encodeUtf8) secr
    lease0 = maybe Null (Int . fromIntegral) lease


registerFeed :: RedisCtx m f => B.ByteString -> m (f Status)
registerFeed topic = set (B.append "known_feed:" topic) "1"

knownFeed :: RedisCtx m f => B.ByteString -> m (f Bool)
knownFeed topic = exists (B.append "known_feed:"  topic)

initSubscriberCounter :: RedisCtx m f => B.ByteString -> m (f Status)
initSubscriberCounter topic = set (B.append "subs:" topic) "0"

incrSubscriberCount :: RedisCtx m f => B.ByteString -> m (f Integer)
incrSubscriberCount topic = incr (B.append "subs:" topic)

subscriberCount :: (RedisCtx m f, Functor f)
                => B.ByteString
                -> m (f (Maybe Integer))
subscriberCount topic = liftM (((decode . fromStrict) <$>) <$>)
                        (get $ B.append "subs:" topic)

validatePublishRequest :: S.Text -> Redis (Either Reply (Maybe (Pub Submitted)))
validatePublishRequest url = maybe checks (const nothing) (validateUrl url)
  where
    encodedUrl = encodeUtf8 url

    checks = bindRedis checkExist $ knownFeed encodedUrl

    checkExist exist
      | exist     = bindRedis checkCount $ subscriberCount encodedUrl
      | otherwise = nothing

    checkCount (Just count)
      | count > 0 = just =<< makePub Submitted url
      | otherwise = nothing
    checkCount _ = error "Impossible situation in validationPublishRequest"

    nothing = returnRedis Nothing
    just    = returnRedis . Just

makePub :: (MonadIO m, Publishing p) => p -> S.Text -> m (Pub p)
makePub state url = liftIO $ fmap (Pub state . PubInfos url 1) getCurrentTime

returnRedis :: a -> Redis (Either Reply a)
returnRedis = return . Right

mapRedis :: (a -> b) -> Redis (Either Reply a) -> Redis (Either Reply b)
mapRedis f = fmap (fmap f)

bindRedis :: (a -> Redis (Either Reply b))
          -> Redis (Either Reply a)
          -> Redis (Either Reply b)
bindRedis f m = (either (return . Left . id) f) =<< m

unitRedis :: Redis (Either Reply a) -> Redis (Either Reply ())
unitRedis = mapRedis (const ())

-- What a shame this isn't in stlib
instance Foldable (Either a) where
    foldMap f (Right a) = f a
    foldMap _ _         = mempty

instance Traversable (Either a) where
    traverse f (Right b) = Right <$> (f b)
    traverse _ (Left a)  = pure (Left a)
