{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Web.Deiko.Hub.Persist where

import           Web.Deiko.Hub.Http         (verify)
import           Web.Deiko.Hub.Parse        (validateUrl)
import           Web.Deiko.Hub.Types

import           Control.Applicative
import           Control.Exception          (ioError)
import           Control.Monad.Error
import           Control.Monad.Reader       (MonadReader, ReaderT (..), asks)
import           Control.Monad.Trans
import           Control.Monad.Trans.Either

import           Data.Binary                hiding (get)
import           Data.Binary.Put
import qualified Data.ByteString            as B
import           Data.ByteString.Char8      (pack, unpack)
import           Data.ByteString.Lazy       (fromStrict, toStrict)
import           Data.Foldable
import           Data.Functor.Identity      (Identity (..))
import           Data.Hashable
import           Data.Int                   (Int8)
import           Data.Monoid                (Monoid (..))
import           Data.String
import qualified Data.Text                  as S
import           Data.Text.Encoding         (encodeUtf8)
import           Data.Time.Clock            (UTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX      (utcTimeToPOSIXSeconds)
import           Data.Traversable

import           Database.Redis             hiding (decode)
import           Database.SQLite            hiding (Status)

import           System.Directory
import           System.IO.Error            (userError)

import           Text.Atom.Feed             (Feed (..))
import           Text.Atom.Feed.Export      (xmlFeed)
import           Text.Atom.Feed.Import      (elementFeed)
import           Text.Deiko.Config          (CanReport, Config, getValue,
                                             loadConfig)
import           Text.XML.Light             (parseXMLDoc, showElement)


data DbOpts = DbOpts { dbLocation        :: String
                     , dbFeedByIdQuery   :: String
                     , dbInsertSubQuery  :: String
                     , dbInsertFeedQuery :: String
                     , dbDeleteSubQuery  :: String }

loadDbOpts :: (MonadIO m, CanReport m) => m DbOpts
loadDbOpts = do
  config <- loadConfig "conf/db.conf"
  liftM5 DbOpts
           (getValue "db.name" config)
           (getValue "db.queries.feed-id" config)
           (getValue "db.queries.insert.sub" config)
           (getValue "db.queries.insert.feed" config)
           (getValue "db.queries.delete.sub" config)

executeRedis :: (MonadIO m, MonadError HubError m)
             => Redis (Either Reply a)
             -> m a
executeRedis action = do
  result <- liftIO $
            connect defaultConnectInfo >>=
                        (flip runRedis $ action)
  either (throwError . InternalError . fromString . show) return result

eventLoop :: (CanReport m, MonadIO m)
          => (forall n. (MonadIO n, MonadReader DbOpts n) => B.ByteString -> n a)
          -> (forall n. (MonadIO n, MonadReader DbOpts n) => Sub Verified -> n b)
          -> (forall n. (MonadIO n, MonadReader DbOpts n) => Sub (Deletion Verified) -> n c)
          -> (forall n. (MonadIO n, MonadReader DbOpts n) => Feed -> n d)
          -> m ()
eventLoop onPublish onSub onDel onFetch = do
  opts <- loadDbOpts
  liftIO $
         connect defaultConnectInfo >>= \handle ->
         runRedis handle $
         pubSub (subscribe channels) (go opts)
  where
    go opts (Message channel msg) =
      case channel of
        "publish"     -> unit $ runReaderT (onPublish msg) opts
        "async_sub"   -> unit (fromByteString msg >>= \sub ->
                                 runReaderT (verifying onSub sub) opts)
        "async_unsub" -> unit (fromByteString msg >>= \sub ->
                                 runReaderT (verifying onDel sub) opts)
        "fetched"     -> unit $
                         do result <- runEitherT $ runReaderT
                                      (withSqliteConnection (getFeedById (unpack msg)))
                                      opts
                            case result of
                              Left msg   -> ioError $ userError (show msg)
                              Right feed -> maybe (return ())
                                            (\f ->
                                             liftM (const ())
                                             (runReaderT (onFetch f) opts))
                                            feed


    unit m = m >> return mempty

    void _  = return ()

    verifying f e = do (_, res) <- verify e
                       maybe (return ()) (((return ()) <*) . f) res

    channels = ["publish"
               ,"async_sub"
               ,"async_unsub"
               ,"fetched"]

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

pushAsyncSubRequest :: (Pending v, ToValue v, Async (Sub v), RedisCtx m f)
                    => Sub v
                    -> m (f Integer)
pushAsyncSubRequest s =
  publish (B.append "async_" (asyncKey s)) (toByteString s)

pushPublishQueue :: RedisCtx m f => Pub Submitted -> m (f Integer)
pushPublishQueue (Pub _ (PubInfos url _ _)) =
  publish "publish" (encodeUtf8 url)

publishFeed :: RedisCtx m f => Feed -> m (f Integer)
publishFeed feed = publish "fetched" (pack $ feedId $ feed)

withSqliteConnection :: (MonadIO m, MonadError HubError m, MonadReader DbOpts m)
                     => (SQLiteHandle -> m a)
                     -> m a
withSqliteConnection f = do
  location <- asks dbLocation
  handle   <- liftIO $ getCurrentDirectory >>=
              \dir -> openConnection (dir ++ location)
  a        <- f handle
  liftIO $ closeConnection handle
  return a

sqliteExecParamStmt :: (MonadIO m, MonadError HubError m, SQLiteResult a)
                    => String
                    -> [(String, Value)]
                    -> (SQLiteHandle -> m [[Row a]])
sqliteExecParamStmt query params handle =
  go =<< (liftIO $ execParamStatement handle query params)
    where
      go = either (throwError . InternalError . fromString) return

sqliteExecParamStmt_ :: (MonadIO m, MonadError HubError m)
                     => String
                     -> [(String, Value)]
                     -> (SQLiteHandle -> m ())
sqliteExecParamStmt_ query params handle =
  go =<< (liftIO $ execParamStatement_ handle query params)
    where
      go = maybe (return ()) (throwError . InternalError . fromString)

unregisterSub :: (MonadIO m, MonadError HubError m, MonadReader DbOpts m
                 , Ended v w)
              => Sub w
              -> (SQLiteHandle -> m ())
unregisterSub sub handle = do
  query <- asks dbDeleteSubQuery
  sqliteExecParamStmt_ query
                         (deleteSubParams (subParams $ subInfos sub))
                         handle

deleteSubParams :: SubParams -> [(String, Value)]
deleteSubParams (SubParams cb _ topic _ _ _ _) =
  [(":topic", Blob $ encodeUtf8 topic)
  ,(":callback", Blob $ encodeUtf8 cb)]

registerSub :: (MonadIO m, MonadError HubError m, MonadReader DbOpts m
               , Ended v w)
            => Sub w
            -> (SQLiteHandle -> m ())
registerSub sub handle = do
  query <- asks dbInsertSubQuery
  sqliteExecParamStmt_ query
                         (registerSubParams $ subParams $ subInfos sub)
                         handle

registerSubParams :: SubParams -> [(String, Value)]
registerSubParams (SubParams cb _ topic _ lease secr _) =
  [(":topic", Blob $ encodeUtf8 topic)
  ,(":callback", Blob $ encodeUtf8 cb)
  ,(":lease", lease0)
  ,(":secret", secret)]
  where
    secret = maybe Null (Blob . encodeUtf8) secr
    lease0 = maybe Null (Int . fromIntegral) lease

saveFetchedFeed :: (MonadIO m, MonadError HubError m, MonadReader DbOpts m)
                => UTCTime
                -> Feed
                -> (SQLiteHandle -> m ())
saveFetchedFeed time feed handle = do
  query <- asks dbInsertFeedQuery
  sqliteExecParamStmt_ query (feedParams time feed) handle

feedParams :: UTCTime -> Feed -> [(String, Value)]
feedParams time feed =
  let content = pack $ showElement $ xmlFeed feed
  in [(":feed_id", Text (feedId feed))
     ,(":fetch_date", Int (round $ utcTimeToPOSIXSeconds time))
     ,(":xml", Blob content)]

getFeedByIdParams :: String -> [(String, Value)]
getFeedByIdParams fId = [(":feed_id", Text fId)]

getFeedById :: (MonadIO m, MonadError HubError m, MonadReader DbOpts m)
            => String
            -> (SQLiteHandle -> m (Maybe Feed))
getFeedById fId handle = do
  query  <- asks dbFeedByIdQuery
  result <- f query handle
  case result of
    (((("xml", Text xml):_):_):_) -> return (elementFeed =<< parseXMLDoc xml)
    _                             -> return Nothing

  where
    f query = sqliteExecParamStmt query (getFeedByIdParams fId)

registerFeed :: RedisCtx m f => B.ByteString -> m (f Status)
registerFeed topic = set (B.append "known_feed:" topic) "1"

knownFeed :: RedisCtx m f => B.ByteString -> m (f Bool)
knownFeed topic = exists (B.append "known_feed:"  topic)

incrSubscriberCount :: RedisCtx m f => B.ByteString -> m (f Integer)
incrSubscriberCount topic = incr (B.append "subs:" topic)

subscriberCount :: (RedisCtx m f, Functor f)
                => B.ByteString
                -> m (f (Maybe Int8))
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
