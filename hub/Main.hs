{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

import           Web.Deiko.Hub.Parse
import           Web.Deiko.Hub.Persist
import           Web.Deiko.Hub.Types

import           Control.Applicative
import           Control.Arrow              ((***))
import           Control.Monad
import           Control.Monad.Trans

import           Control.Monad.Error
import           Control.Monad.Trans.Either

import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as L
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
         get "/test" readFromRedis

    where
      go :: B.ByteString -> ActionM ()
      go "subscribe" = subscription
      go "publish"   = publish
      go _           = status status404

errorHandle :: HubError -> ActionM ()
errorHandle (BadRequest e)     = status status400 >> text e
errorHandle VerificationFailed = status status400 >> text "Verification failed"
errorHandle (ParseError e)     = status status400 >> text e
errorHandle (InternalError e)  = status status500 >> text e >> (liftIO $ print e)

subscription :: ActionM ()
subscription = do
  xs     <- params
  report <- runEitherT $ process $ fmap (T.toStrict *** T.toStrict) xs
  either errorHandle status report
    where
      process params = do request <- parseSubParams params
                          sub     <- makeSubscription NotVerified request
                          let callback = encodeUtf8 (subCallback request)
                              topic    = encodeUtf8 (subTopic request)
                          executeRedis $ saveSubscription sub
                          verifyRequest request

publish :: ActionM ()
publish = do
  url  <- param "hub.url"
  maybe (persistPublishRequest url) (errorHandle . ParseError . T.fromStrict . decodeUtf8) (validateUrl url)
    where
      persistPublishRequest url = do
            let history    = pushPublishEvent url Publish
                queue      = pushPublishQueue url
            result <- runEitherT $ executeRedis $ history >> queue
            either errorHandle (const $ return ()) result

-- testing
readFromRedis :: ActionM()
readFromRedis = do
  callback <- param "hub.callback"
  report   <- runEitherT $ process callback
  either errorHandle (text . fromString . show) (report :: Either HubError [Subscription])
      where
        process callback = do bytes <- executeRedis $ loadSubscriptionHistory callback "http://www.google.com"
                              traverse fromByteString bytes

makeSubscription :: (MonadIO m, Applicative m) => SubState -> SubParams -> m Subscription
makeSubscription state params = (Sub 1 state params) <$> currentTime
    where
      currentTime = liftIO getCurrentTime

randomString :: (MonadIO m, IsString s) => m s
randomString = return "test_challenge"

asyncSubQueueLoop :: (SubParams -> IO a) -> IO ()
asyncSubQueueLoop handle = do result <- runEitherT $ executeRedis popAsyncSubRequest
                              either (print . show) go result
                                  where
                                    go = maybe (return ()) ((asyncSubQueueLoop handle <*) . handle)
 
asyncVerification :: IO ()
asyncVerification = asyncSubQueueLoop ((go =<<) . runEitherT . verifyRequest)
    where
      go = either (print . show) (const $ return ())

verifyRequest :: (MonadIO m, MonadError HubError m, Applicative m) => SubParams -> m Status
verifyRequest req@(SubParams callback mode topic verify _ _ _) = go verify
    where
      go ("async":_) = executeRedis (pushAsyncSubRequest req) >> return status202
      go ("sync":_)  = do challenge <- randomString
                          response  <- liftIO $ (curlGetResponse_ (url $ query $ parameters challenge) [] :: IO (CurlResponse_ [(String, String)] B.ByteString))
                          case (respStatus response, respBody response) of
                            (status, back)
                                | 200 <= status && status < 300 && back == (encodeUtf8 challenge) ->
                                    do let encodedTopic = encodeUtf8 topic
                                       sub <- makeSubscription Verified req 
                                       executeRedis $ do saveSubscription sub
                                                         let proceed exist
                                                                 | exist     = (const () <$>) <$> incrSubscriberCount encodedTopic 
                                                                 | otherwise = (const () <$>) <$> registerFeed encodedTopic <* initSubscriberCounter encodedTopic
                                                         (either (return . Left) proceed) =<< knownFeed encodedTopic
                                       withSqliteConnection $ \handle ->
                                           let unregister      = unregisterSubscription handle sub
                                               register        = registerSubscription handle sub
                                           in unregister >> register
                                       return status204
                            _ -> throwError VerificationFailed
      go (_:xs)      = go xs
      go []          = throwError $ BadRequest "Unsupported verification mode"
      url query = S.unpack $ ((S.append callback) . (S.append "?")) query
      query xs = foldl1 (\a b -> S.append a (S.append "&" b)) xs
      parameters challenge = [S.append "hub.mode" $ S.append "=" mode
                             ,S.append "hub.topic" $ S.append "=" topic
                             ,S.append "hub.challenge" $ S.append "=" challenge
                             ,"hub.lease_seconds=10"]

fetchContent :: B.ByteString -> IO B.ByteString
fetchContent = error "todo"

instance Applicative ActionM where
    pure = return
    (<*>) = ap
