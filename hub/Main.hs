{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TupleSections         #-}

module Main (main, asyncQueueMain) where

import           Web.Deiko.Hub.Http         (fetchContent, verify)
import           Web.Deiko.Hub.Parse
import           Web.Deiko.Hub.Persist
import           Web.Deiko.Hub.Types

import           Control.Applicative
import           Control.Arrow              ((&&&), (***))
import           Control.Monad
import           Control.Monad.Trans

import           Control.Monad.Error
import           Control.Monad.Reader       (MonadReader (..), ReaderT (..))
import           Control.Monad.Trans.Either

import qualified Data.ByteString            as B
import           Data.ByteString.Char8      (unpack)
import qualified Data.ByteString.Lazy       as L
import           Data.Conduit
import           Data.Foldable              (foldMap, traverse_)
import           Data.Monoid                ((<>))
import           Data.String                (IsString (..))
import qualified Data.Text                  as S
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy             as T
import           Data.Time.Clock            (getCurrentTime)
import           Data.Traversable           (traverse)

import           Network.HTTP.Types.Status

import           System.Log.Formatter
import           System.Log.Handler         hiding (setLevel)
import           System.Log.Handler.Simple
import           System.Log.Logger

import           Text.Atom.Feed             (Feed (..))
import           Text.Deiko.Config          (CanReport (..), ConfigError (..))

import           Web.Scotty

instance CanReport ScottyM where
  configError = liftIO . configError

type HubAction a = ReaderT DbOpts ActionM a

main = do
  opts    <- loadDbOpts
  handler <- fileHandler "hub.log" INFO
  let formatter = simpleLogFormatter "$time:[$tid]:[$prio]:$loggername> $msg"
      fHandler  = setFormatter handler formatter
  updateGlobalLogger rootLoggerName (setLevel INFO . setHandlers [fHandler])
  scotty 3000 $ do
         post "/hub" $ go opts =<< param "hub.mode"

    where
      go :: DbOpts -> B.ByteString -> ActionM ()
      go opts "subscribe"   = runReaderT (subscription updateSubFigures) opts
      go opts "unsubscribe" = runReaderT (subscription confirmUnsub) opts
      go _    "publish"     = publish
      go _ _                = status status404

asyncQueueMain :: (CanReport m, MonadIO m) => m ()
asyncQueueMain = eventLoop fetching
                 (runEitherT . updateSubFigures)
                 (runEitherT . confirmUnsub)
                 (distribute)
  where
    fetching bytes =
      do res  <- fetchContent (unpack bytes)
         unwrapMonad $ traverse_ (WrapMonad . runEitherT . process) res

    process feed =
      do time <- liftIO getCurrentTime
         withSqliteConnection (saveFetchedFeed time feed)
         executeRedis $ publishFeed feed

    distribute feed =
      let prod        = runSqliteStmt (loadSubs (feedId feed))
          action opts = prod opts $$ printer in
      do opts <- ask
         liftIO $ print (feedId feed)
         liftIO $ runResourceT (action opts)

errorHandle :: HubError -> ActionM ()
errorHandle (BadRequest e)     = do
  liftIO $ infoM "Main" ("BadRequest: " ++ (show e))
  status status400
  text e
errorHandle VerificationFailed = status status400 >> text "Verification failed"
errorHandle (ParseError e)     = status status400 >> text e
errorHandle (InternalError e)  = do
  liftIO $ errorM "Main" (show e)
  status status500

subscription :: (Start v, Pending v, ToValue v, Ended v w, Verification v
                , Async (Sub v), Verification w)
             => (forall m. (MonadIO m, MonadError HubError m, MonadReader DbOpts m) => Sub w -> m a)
             -> HubAction ()
subscription whenVerified = do
  xs     <- lift params
  liftIO $ infoM "Main" (logParams xs)
  report <- runEitherT $ process $ fmap (T.toStrict *** T.toStrict) xs
  liftIO $ infoM "Main" "Verification passed"
  either (lift . errorHandle) (lift . status) report
    where
      process params = do request <- parseSubParams params
                          sub     <- makeSub start request
                          let callback = encodeUtf8 (subCallback request)
                              topic    = encodeUtf8 (subTopic request)
                          executeRedis $ saveSubscription sub
                          result  <- verification sub
                          case result of
                            (status, verifiedSub) ->
                                traverse_ whenVerified verifiedSub >>
                                          return status
      logParams params =
        "\nSubscription handler\n" ++
        foldMap (\(p, v) -> (show p) ++ ": " ++ (show v) ++ "\n") params

publish :: ActionM ()
publish = do
  url  <- param "hub.url"
  maybe (persistPublishRequest (T.toStrict url))
          (errorHandle . ParseError) (validateUrl url)
    where
      persistPublishRequest url = do
            let persist (Just pub) = unitRedis $
                                     pushPublishQueue pub
                persist _          = returnRedis ()
            result <- runEitherT $ executeRedis $
                      bindRedis persist (validatePublishRequest url)
            either errorHandle (const (status status204)) result

verification :: (MonadIO m, MonadError HubError m, Applicative m
                , Pending v, ToValue v, Async (Sub v), Ended v w, Verification w)
             => Sub v
             -> m (Status, Maybe (Sub w))
verification sub = go $ subVerify $ subParams $ subInfos sub
  where
    go ("async":_) = executeRedis (pushAsyncSubRequest sub) >>
                     return (status202, Nothing)
    go ("sync":_)  = verify sub
    go (_:xs)      = go xs
    go []          = throwError $ BadRequest "Unsupported verification mode"

updateSubFigures :: (MonadIO m, MonadError HubError m, MonadReader DbOpts m)
                 => Sub Verified -> m ()
updateSubFigures sub@(Sub _ (SubInfos _ params _)) =
  let encodedTopic = encodeUtf8 $ subTopic params
      incr = unitRedis (incrSubscriberCount encodedTopic)
      init = unitRedis (registerFeed encodedTopic)
      proceed exist
        | exist     = incr
        | otherwise = init >> incr
      action sub = saveSubscription sub >>
                   bindRedis proceed (knownFeed encodedTopic)
      registration handle = unregisterSub sub handle >>
                            registerSub sub handle
  in do executeRedis $ action sub
        withSqliteConnection registration

confirmUnsub :: (MonadIO m, MonadError HubError m, MonadReader DbOpts m)
             => Sub (Deletion Verified)
             -> m ()
confirmUnsub sub = (executeRedis $ saveSubscription sub) >>
                   withSqliteConnection (unregisterSub sub)
