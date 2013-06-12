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

import           Web.Scotty

main = do
  opts <- loadDbOpts
  scotty 3000 $
         post "/hub" $ go (HubOpts opts) =<< param "hub.mode"

    where
      go :: HubOpts -> B.ByteString -> ActionM ()
      go opts "subscribe"   = subHandler updateSubFigures opts
      go opts "unsubscribe" = subHandler confirmUnsub opts
      go _    "publish"     = publish
      go _ _                = status status404

      subHandler f opts = do
         xs       <- params
         (s, msg) <- liftIO $ subscription
                     f opts (fmap (T.toStrict *** T.toStrict) xs)
         status s
         unwrapMonad $ traverse_ (WrapMonad . text) msg

asyncQueueMain :: IO ()
asyncQueueMain = eventLoop fetching
                 updateSubFigures
                 confirmUnsub
                 distribute
  where
    fetching opts bytes =
      do res  <- fetchContent (unpack bytes)
         unwrapMonad $ traverse_ (WrapMonad . process opts) res

    process opts feed =
      do time <- getCurrentTime
         withSqliteConnection (saveFetchedFeed time feed (hubDbOpts opts))
                                (hubDbOpts opts)
         executeRedis $ publishFeed feed

    distribute opts feed =
      let dbOpts = hubDbOpts opts
          prod   = runSqliteStmt (loadSubs (feedId feed))
          action = prod dbOpts $$ printer in
      do print (feedId feed)
         runResourceT action

subscription :: (Start v, Pending v, ToValue v, Ended v w, Verification v
                , Async (Sub v), Verification w)
             => (HubOpts -> Sub w -> IO (Either String a))
             -> HubOpts
             -> [(S.Text, S.Text)]
             -> IO (Status, Maybe T.Text)
subscription callback opts params =
  let action          = traverse afterParse (parseSubParams params)
      internalError _ = return (status500, Nothing)
      badRequest e    = return (status400, Just $ formatError e)
      simple s        = return (s, Nothing)

      formatError =
        T.fromStrict . foldMap (\(MError key msg) -> key <> ": " <> msg <> "\n")

      verifying sub _ =
        do (s, vsub) <- verification sub
           res       <- traverse (callback opts) vsub
           maybe (simple s)
                   (either internalError (const $ simple s))
                   res

      afterParse request =
        do sub <- makeSub start request
           res <- executeRedis $ saveSubscription sub
           either internalError (verifying sub) res

  in either badRequest return =<< action

publish :: ActionM ()
publish = do
  url  <- param "hub.url"
  maybe (persistPublishRequest (T.toStrict url))
          (const $ status status400) (validateUrl url)
    where
      persistPublishRequest url = do
            let persist (Just pub) = unitRedis $
                                     pushPublishQueue pub
                persist _          = returnRedis ()
            res <- liftIO $ executeRedis $
                   bindRedis persist (validatePublishRequest url)
            either (const $ status status500)
                     (const (status status204)) res

verification :: (Pending v, ToValue v, Async (Sub v)
                , Ended v w, Verification w)
             => Sub v
             -> IO (Status, Maybe (Sub w))
verification sub = go $ subVerify $ subParams $ subInfos sub
  where
    go ("async":_) =
      fmap
      (either (\_ -> (status500, Nothing)) (\_ -> (status202, Nothing)))
      (executeRedis (pushAsyncSubRequest sub))

    go ("sync":_)  = verify sub
    go (_:xs)      = go xs
    go []          = return (status400, Nothing)

updateSubFigures :: HubOpts -> Sub Verified -> IO (Either String ())
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

confirmUnsub :: HubOpts -> Sub (Deletion Verified) -> IO (Either String ())
confirmUnsub opts sub =
  let dbOpts = hubDbOpts opts in
  (executeRedis $ saveSubscription sub) >>
  withSqliteConnection (unregisterSub dbOpts sub) dbOpts
