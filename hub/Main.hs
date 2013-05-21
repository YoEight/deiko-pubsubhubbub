{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TupleSections         #-}

import           Web.Deiko.Hub.Http         (fetchContent, verify)
import           Web.Deiko.Hub.Parse
import           Web.Deiko.Hub.Persist
import           Web.Deiko.Hub.Types

import           Control.Applicative
import           Control.Arrow              ((&&&), (***))
import           Control.Monad
import           Control.Monad.Trans

import           Control.Monad.Error
import           Control.Monad.Trans.Either

import qualified Data.ByteString            as B
import           Data.ByteString.Char8      (unpack)
import qualified Data.ByteString.Lazy       as L
import           Data.Foldable              (traverse_)
import           Data.Monoid                ((<>))
import           Data.String
import qualified Data.Text                  as S
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy             as T
import           Data.Time.Clock            (getCurrentTime)
import           Data.Traversable           (traverse)

import           Network.HTTP.Types.Status
import           Web.Scotty

main = scotty 3000 $ do
         post "/hub" $ go =<< param "hub.mode"

    where
      go :: B.ByteString -> ActionM ()
      go "subscribe"   = subscription updateSubFigures
      go "unsubscribe" = subscription confirmUnsub
      go "publish"     = publish
      go _             = status status404

asyncQueueMain :: MonadIO m => m ()
asyncQueueMain = eventLoop fetching
                 (runEitherT . updateSubFigures)
                 (runEitherT . confirmUnsub)
  where
    fetching bytes = 
      do res  <- fetchContent (unpack bytes)
         time <- liftIO getCurrentTime
         maybe (return $ Right ()) 
                 (runEitherT . withSqliteConnection . saveFetchedFeed time)
                 res

errorHandle :: HubError -> ActionM ()
errorHandle (BadRequest e)     = status status400 >> text e
errorHandle VerificationFailed = status status400 >> text "Verification failed"
errorHandle (ParseError e)     = status status400 >> text e
errorHandle (InternalError e)  = status status500 >> text e >> (liftIO $ print e)

subscription :: (Start v, Pending v, ToValue v, Ended v w, Verification v
                , Async (Sub v), Verification w)
             => (forall m. (MonadIO m, MonadError HubError m) => Sub w -> m a)
             -> ActionM ()
subscription whenVerified = do
  xs     <- params
  report <- runEitherT $ process $ fmap (T.toStrict *** T.toStrict) xs
  either errorHandle status report
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
            either errorHandle return result

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

updateSubFigures :: (MonadIO m, MonadError HubError m) => Sub Verified -> m ()
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

confirmUnsub :: (MonadIO m, MonadError HubError m)
             => Sub (Deletion Verified)
             -> m ()
confirmUnsub sub = (executeRedis $ saveSubscription sub) >>
                   withSqliteConnection (unregisterSub sub)
