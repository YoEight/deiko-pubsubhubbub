{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TupleSections         #-}

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
import qualified Data.ByteString.Lazy       as L
import           Data.Foldable              (traverse_)
import           Data.Monoid                ((<>))
import           Data.String
import qualified Data.Text                  as S
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy             as T
import           Data.Time.Clock            (getCurrentTime)
import           Data.Traversable           (traverse)

import           Network.Curl
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

errorHandle :: HubError -> ActionM ()
errorHandle (BadRequest e)     = status status400 >> text e
errorHandle VerificationFailed = status status400 >> text "Verification failed"
errorHandle (ParseError e)     = status status400 >> text e
errorHandle (InternalError e)  = status status500 >> text e >> (liftIO $ print e)

subscription :: (Start v, Pending v, ToValue v, Ended v w, Verification v
                , Verification w)
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
                                     pushPublishEvent pub *>
                                     pushPublishQueue pub
                persist _          = returnRedis ()
            result <- runEitherT $ executeRedis $
                      bindRedis persist (validatePublishRequest url)
            either errorHandle return result

makeSub :: (MonadIO m, Applicative m, Verification v)
        => v
        -> SubParams
        -> m (Sub v)
makeSub v params = ((Sub v) . (SubInfos 1 params)) <$> currentTime
  where
    currentTime = liftIO getCurrentTime

randomString :: (MonadIO m, IsString s) => m s
randomString = return "test_challenge"

asyncSubQueueLoop :: (forall m. MonadIO m => Sub Verified -> m a)
                  -> (forall m. MonadIO m => Sub (Deletion Verified) -> m b)
                  -> IO ()
asyncSubQueueLoop fsub usub =
  let verifying f e =
        do (_, res) <- verify e
           maybe unit ((unit <*) . f) res

      unit       = return ()
      onSubscr   = verifying fsub
      onDeletion = verifying usub
      onSuccess  = maybe unit (either onSubscr onDeletion)
      logError   = print . show
  in (runEitherT $ executeRedis popAsyncSubRequest) >>=
       ((asyncSubQueueLoop fsub usub <*) . either logError onSuccess)

asyncVerification :: IO ()
asyncVerification =
  asyncSubQueueLoop
  (runEitherT . updateSubFigures)
  (runEitherT . confirmUnsub)

verification :: (MonadIO m, MonadError HubError m, Applicative m
                , Pending v, Ended v w, Verification w)
             => Sub v
             -> m (Status, Maybe (Sub w))
verification sub = go $ subVerify $ subParams $ subInfos sub
  where
    go ("async":_) = executeRedis (pushAsyncSubRequest sub) >>
                     return (status202, Nothing)
    go ("sync":_)  = verify sub
    go (_:xs)      = go xs
    go []          = throwError $ BadRequest "Unsupported verification mode"

type CurlResp = IO (CurlResponse_ [(String, String)] B.ByteString)

verify :: (MonadIO m, Applicative m, Pending v, Ended v w, Verification w)
       => Sub v
       -> m (Status, Maybe (Sub w))
verify = go . subParams . subInfos
  where
    go params = do
      challenge <- randomString
      let encChallenge = encodeUtf8 challenge
          url          = S.unpack (subParamsQuery params challenge)
      response  <- liftIO $ curl url
      case (respStatus response, respBody response) of
        (status, back)
          | 200 <= status && status < 300 && back == encChallenge ->
            fmap ((status204,) . Just) (makeSub end params)
        _ -> return (status400, Nothing)

    curl :: String -> CurlResp
    curl = flip curlGetResponse_ $ []

subParamsQuery :: SubParams -> S.Text -> S.Text
subParamsQuery (SubParams callback mode topic verify _ _ _) challenge =
  callback <> "?" <> foldl1 (\a b -> a <> "&" <> b) params
    where
      params = ["hub.mode=" <> mode
               ,"hub.topic=" <> topic
               ,"hub.challenge=" <> challenge
               ,"hub.lease_seconds=10"]

updateSubFigures :: (MonadIO m, MonadError HubError m) => Sub Verified -> m ()
updateSubFigures sub@(Sub _ (SubInfos _ params _)) =
  let encodedTopic = encodeUtf8 $ subTopic params
      incr = unitRedis (incrSubscriberCount encodedTopic)
      init = unitRedis
             (registerFeed encodedTopic <* initSubscriberCounter encodedTopic)
      proceed exist
        | exist     = incr
        | otherwise = init
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

fetchContent :: B.ByteString -> IO B.ByteString
fetchContent = error "todo"

instance Applicative ActionM where
    pure = return
    (<*>) = ap
