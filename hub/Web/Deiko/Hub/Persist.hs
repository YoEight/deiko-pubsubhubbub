{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Web.Deiko.Hub.Persist where

import           Web.Deiko.Hub.Http         (verify)
import           Web.Deiko.Hub.Parse        (validateUrl)
import           Web.Deiko.Hub.Types

import           Control.Applicative
import           Control.Exception          (ioError, throw)
import           Control.Monad.Error
import           Control.Monad.Reader       (MonadReader (..), ReaderT (..),
                                             asks)
import           Control.Monad.Trans
import           Control.Monad.Trans.Either

import           Data.Binary                hiding (get)
import           Data.Binary.Put
import qualified Data.ByteString            as B
import           Data.ByteString.Char8      (pack, unpack)
import           Data.ByteString.Lazy       (fromStrict, toStrict)
import           Data.Conduit
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

loadDbOpts :: (CanReport m, MonadIO m) => m DbOpts
loadDbOpts = do
  config      <- loadConfig "conf/db.conf"
  location    <- getValue "db.name" config
  getFeedId   <- getValue "db.queries.feed-id" config
  insertSub   <- getValue "db.queries.insert.sub" config
  insertFeed  <- getValue "db.queries.insert.feed" config
  deleteSub   <- getValue "db.queries.delete.sub" config
  getSubTopic <- getValue "db.queries.sub-topic" config
  return (DbOpts location getFeedId insertSub insertFeed deleteSub getSubTopic)

executeRedis :: Redis a -> IO a
executeRedis action =
  connect defaultConnectInfo >>= \cnx ->
    runRedis cnx action

eventLoop :: (HubOpts -> B.ByteString -> IO a)
          -> (HubOpts -> Sub Verified -> IO b)
          -> (HubOpts -> Sub (Deletion Verified) -> IO c)
          -> (HubOpts -> Feed -> IO d)
          -> IO ()
eventLoop onPublish onSub onDel onFetch = do
  opts <- loadDbOpts
  liftIO $
         connect defaultConnectInfo >>= \handle ->
         runRedis handle $
         pubSub (subscribe channels) (go (HubOpts opts))
  where
    go opts (Message channel msg) =
      case channel of
        "publish"     -> unit $ onPublish opts msg
        "async_sub"   -> unit (fromByteString msg >>= \sub ->
                                 verifying (onSub opts) sub)
        "async_unsub" -> unit (fromByteString msg >>= \sub ->
                                 verifying (onDel opts) sub)
        "fetched"     -> unit $
                         do feed <- fetchAction opts msg
                            maybe (return ())
                                    (\f -> void $ onFetch opts f)
                                    feed

    fetchAction opts msg =
      let prod   = runSqliteStmt (getFeedById (unpack msg)) (hubDbOpts opts)
          action = prod $$ rowToFeed in
      runResourceT action

    unit m = m >> return mempty

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

withSqliteConnection :: (SQLiteHandle -> IO a) -> DbOpts -> IO a
withSqliteConnection f opts = do
  handle   <- openConnection (dbLocation opts)
  a        <- f handle
  liftIO $ closeConnection handle
  return a

runSqliteStmt ::(DbOpts -> SQLiteHandle -> Producer (ResourceT IO) a)
              -> DbOpts
              -> Producer (ResourceT IO) a
runSqliteStmt k opts =
  bracketP (openConnection (dbLocation opts))
           closeConnection (k opts)

type DbAction a = DbOpts -> SQLiteHandle -> Producer (ResourceT IO) a

loadSubs :: String -> DbAction [Row Value]
loadSubs topic opts handle = do
  result <- liftIO $ execParamStatement handle (dbSubByTopic opts) params
  either (liftIO . ioError . userError) (traverse_ yield) result
  where
    params = [(":topic", Text topic)]

printer :: (Show a, MonadIO m) => Consumer a m ()
printer = awaitForever (liftIO . print)

sqliteExecParamStmt_ :: String
                     -> [(String, Value)]
                     -> (SQLiteHandle -> IO (Either String ()))
sqliteExecParamStmt_ query params handle =
  go =<< execParamStatement_ handle query params
    where
      go = return . maybe (Right ()) Left

unregisterSub :: Ended v w
              => DbOpts
              -> Sub w
              -> (SQLiteHandle -> IO (Either String ()))
unregisterSub opts sub handle =
  let query = dbDeleteSubQuery opts in
  sqliteExecParamStmt_ query
                         (deleteSubParams (subParams $ subInfos sub))
                         handle

deleteSubParams :: SubParams -> [(String, Value)]
deleteSubParams (SubParams cb _ topic _ _ _ _) =
  [(":topic", Blob $ encodeUtf8 topic)
  ,(":callback", Blob $ encodeUtf8 cb)]

registerSub :: Ended v w
            => DbOpts
            -> Sub w
            -> (SQLiteHandle -> IO (Either String ()))
registerSub opts sub handle =
  let query = dbInsertSubQuery opts in
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

saveFetchedFeed :: UTCTime
                -> Feed
                -> DbOpts
                -> (SQLiteHandle -> IO (Either String ()))
saveFetchedFeed time feed opts handle =
  let query = dbInsertFeedQuery opts in
  sqliteExecParamStmt_ query (feedParams time feed) handle

feedParams :: UTCTime -> Feed -> [(String, Value)]
feedParams time feed =
  let content = pack $ showElement $ xmlFeed feed
  in [(":feed_id", Text (feedId feed))
     ,(":fetch_date", Int (round $ utcTimeToPOSIXSeconds time))
     ,(":xml", Blob content)]

getFeedByIdParams :: String -> [(String, Value)]
getFeedByIdParams fId = [(":feed_id", Text fId)]

getFeedById :: String -> DbAction [Row Value]
getFeedById feedId opts handle = do
  result <- liftIO $ execParamStatement handle feedId params
  either (liftIO . ioError . userError) (traverse_ yield) result
  where
    params = [(":feed_id", Text feedId)]

rowToFeed :: Consumer [Row Value] (ResourceT IO) (Maybe Feed)
rowToFeed = do
  res <- await
  return ((elementFeed <=< parseXMLDoc) . go =<< res)
  where
    go ((("xml", Text xml):_):_) = xml
-- getFeedById :: (MonadIO m, MonadError HubError m, MonadReader DbOpts m)
--             => String
--             -> (SQLiteHandle -> m (Maybe Feed))
-- getFeedById fId handle = do
--   query  <- asks dbFeedByIdQuery
--   result <- f query handle
--   case result of
--     (((("xml", Text xml):_):_):_) -> return (elementFeed =<< parseXMLDoc xml)
--     _                             -> return Nothing

--   where
--     f query = sqliteExecParamStmt query (getFeedByIdParams fId)

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
