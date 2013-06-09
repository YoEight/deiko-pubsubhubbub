{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TupleSections         #-}

module Main (main, asyncQueueMain) where

import           Web.Deiko.Hub
import           Web.Deiko.Hub.Http         (fetchContent, verify)
import           Web.Deiko.Hub.Parse
import           Web.Deiko.Hub.Persist
import           Web.Deiko.Hub.Types

import           Control.Applicative
import           Control.Arrow              ((&&&), (***))
import           Control.Monad
import           Control.Monad.Trans

import           Control.Monad.Error
import           Control.Monad.Reader       (MonadReader (..), asks)
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

import qualified Web.Scotty                 as Scotty

main = do
  opts <- loadDbOpts
  Scotty.scotty 3000 $
       Scotty.post "/hub" $ go (HubOpts opts) =<< Scotty.param "hub.mode"

    where
      go :: HubOpts -> B.ByteString -> Scotty.ActionM ()
      go opts "subscribe"   = evalHub (subscription updateSubFigures) opts
      go opts "unsubscribe" = evalHub (subscription confirmUnsub) opts
      go _    "publish"     = publish
      go _ _                = Scotty.status status404

asyncQueueMain :: (CanReport m, MonadIO m) => m ()
asyncQueueMain = eventLoop fetching
                 (\o s -> runEitherT $ updateSubFigures o s)
                 (\o s -> runEitherT $ confirmUnsub o s)
                 (distribute)
  where
    fetching opts bytes =
      do res  <- fetchContent (unpack bytes)
         unwrapMonad $ traverse_ (WrapMonad . runEitherT . process opts) res

    process opts feed =
      do time <- liftIO getCurrentTime
         withSqliteConnection (saveFetchedFeed time feed (hubDbOpts opts))
                                (hubDbOpts opts)
         executeRedis $ publishFeed feed

    distribute opts feed =
      let dbOpts = hubDbOpts opts
          prod   = runSqliteStmt (loadSubs (feedId feed))
          action = prod dbOpts $$ printer in
      do liftIO $ print (feedId feed)
         liftIO $ runResourceT action

errorHandle :: HubError -> Hub Scotty.ActionM ()
errorHandle (BadRequest e)     = status status400 >> text e
errorHandle VerificationFailed = status status400 >> text "Verification failed"
errorHandle (ParseError e)     = status status400 >> text e
errorHandle (InternalError e)  = status status500

subscription :: (Start v, Pending v, ToValue v, Ended v w, Verification v
                , Async (Sub v), Verification w)
             => (forall m. (MonadIO m, MonadError HubError m) => HubOpts -> Sub w -> m a)
             -> Hub Scotty.ActionM ()
subscription whenVerified = do
  opts   <- ask
  xs     <- params
  report <- runEitherT $ process opts $ fmap (T.toStrict *** T.toStrict) xs
  either errorHandle status report
    where
      process opts params = do request <- parseSubParams params
                               sub     <- makeSub start request
                               let callback = encodeUtf8 (subCallback request)
                                   topic    = encodeUtf8 (subTopic request)
                               executeRedis $ saveSubscription sub
                               result  <- verification sub
                               case result of
                                 (status, verifiedSub) ->
                                   traverse_ (whenVerified opts) verifiedSub >>
                                           return status
      logParams params =
        "\nSubscription handler\n" ++
        foldMap (\(p, v) -> (show p) ++ ": " ++ (show v) ++ "\n") params

publish :: Scotty.ActionM ()
publish = do
  url  <- Scotty.param "hub.url"
  maybe (persistPublishRequest (T.toStrict url))
          (const $ Scotty.status status400) (validateUrl url)
    where
      persistPublishRequest url = do
            let persist (Just pub) = unitRedis $
                                     pushPublishQueue pub
                persist _          = returnRedis ()
            result <- runEitherT $ executeRedis $
                      bindRedis persist (validatePublishRequest url)
            either (const $ Scotty.status status500)
                     (const (Scotty.status status204)) result

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

updateSubFigures :: (MonadIO m, MonadError HubError m)
                 => HubOpts
                 -> Sub Verified
                 -> m ()
updateSubFigures opts sub@(Sub _ (SubInfos _ params _)) =
  let encodedTopic = encodeUtf8 $ subTopic params
      dbOpts = hubDbOpts opts
      incr = unitRedis (incrSubscriberCount encodedTopic)
      init = unitRedis (registerFeed encodedTopic)
      proceed exist
        | exist     = incr
        | otherwise = init >> incr
      action sub = saveSubscription sub >>
                   bindRedis proceed (knownFeed encodedTopic)
      registration handle = unregisterSub dbOpts sub handle >>
                            registerSub dbOpts sub handle
  in do executeRedis $ action sub
        withSqliteConnection registration dbOpts

confirmUnsub :: (MonadIO m, MonadError HubError m)
             => HubOpts
             -> Sub (Deletion Verified)
             -> m ()
confirmUnsub opts sub =
  let dbOpts = hubDbOpts opts in
  (executeRedis $ saveSubscription sub) >>
  withSqliteConnection (unregisterSub dbOpts sub) dbOpts
