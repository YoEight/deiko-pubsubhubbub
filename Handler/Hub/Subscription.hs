{-# LANGUAGE OverloadedStrings #-}
module  Handler.Hub.Subscription
  (
    subscribe,
    unsubscribe
  ) where

import Import

import Handler.Hub.Util

import Control.Applicative (Applicative(..), (<|>), some)
import Control.Concurrent (forkIO)
import Control.Monad (when)
import qualified Data.ByteString as B
import Data.Conduit
import Data.Foldable (foldMap)
import Data.List.NonEmpty hiding (nonEmpty, insert)
import Data.Maybe (maybeToList)
import Data.Monoid (First(..))
import Data.String (fromString)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Traversable (traverse)
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import Text.Parsec.Text ()
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator hiding (optional)
import Text.ParserCombinators.Parsec.Prim hiding (label, (<|>))

type SubFinal a = KeyBackend (PersistEntityBackend Sub) Sub
                -> Sub
                -> Handler a

subscribe :: Handler Text
subscribe = subscription onSubSuccess

unsubscribe :: Handler Text
unsubscribe = subscription onUnsubSuccess

subscription :: SubFinal () -> Handler Text
subscription k = do
  sub     <- mkRequest
  subId   <- runDB $ creationRoutine sub
  handler <- handlerToIO
  liftIO $ forkIO $ handler (verification sub >> k subId sub)
  accepted
  where
    creationRoutine sub =
      let topic     = subTopic sub
          cb        = subCallback sub
          onExist t = const (entityKey t) <$> replace (entityKey t) sub in do
      res   <- selectFirst [SubTopic ==. topic, SubCallback ==. cb] []
      subId <- maybe (insert sub) onExist res
      date  <- liftIO getCurrentTime
      insert_ (SubHist subId "creation" date)
      return subId

onSubSuccess :: SubFinal ()
onSubSuccess subId sub = runDB action
  where
    action = do
      update subId [SubVerified =. True]
      date <- liftIO getCurrentTime
      insert_ (SubHist subId "verified" date)

onUnsubSuccess :: SubFinal ()
onUnsubSuccess subId sub = runDB action
  where
    action = do
      update subId [SubActivated =. True]
      date <- liftIO getCurrentTime
      insert_ (SubHist subId "deleted" date)

mkRequest :: Handler Sub
mkRequest = do
  callback <- lookupPostParam "hub.callback"
  topic    <- lookupPostParam "hub.topic"
  leaseOpt <- lookupPostParam "hub.lease_seconds"
  secret   <- lookupPostParam "hub.secret"
  action callback topic leaseOpt secret

  where
    validate cb topic lease secret =
      Sub <$>
      url "hub.callback" cb <*>
      url "hub.topic" topic <*>
      validateLease lease   <*>
      pure secret           <*>
      pure False            <*>
      pure True

    action cb topic lease secret =
      validation (invalidArgs . toList) return $
      validate cb topic lease secret

    url key opt = validateUrl key -<< param key opt

randomString :: MonadIO m => m Text
randomString = return "challenge_test"

verification :: Sub -> Handler ()
verification sub = do
  challenge   <- randomString
  (Just mode) <- lookupPostParam "hub.mode"
  req         <- parseUrl (show $ url mode challenge)
  manager     <- fmap httpManager getYesod
  handleResp req mode challenge manager

  where
    url m c =
      let f x    = ("hub.lease_seconds", fromString $ show x)
          lease  = maybeToList $ fmap f (subLeaseSeconds sub)
          params = [("hub.mode", m)
                   ,("hub.topic", subTopic sub)
                   ,("hub.challenge", c)] ++ lease in
      subCallbackUrl params sub

    handleResp req mode challenge manager =
      http req manager >>= \resp ->
        let validStatus s  = status200 <= s && s < status300
            checking check = when (not check) (verificationFailed sub mode)
            sink           = checkChallengeSink (encodeUtf8 challenge) in
        case responseStatus resp of
          s | validStatus s -> checking =<< (responseBody resp $$+- sink)
            | otherwise     -> verificationFailed sub mode

subCallbackUrl :: [(Text, Text)] -> Sub -> Text
subCallbackUrl [] sub     = subCallback sub
subCallbackUrl params sub =
  let cb           = subCallback sub
      sep          = maybe "?" (const "&") (T.find (== '?') cb)
      toStr (k, v) = k <> v in
  cb <> sep <> foldl1 (\a b -> a <> "&" <> b) (fmap toStr params)

checkChallengeSink :: Monad m => B.ByteString -> Sink B.ByteString m Bool
checkChallengeSink challenge = go (B.length challenge) 0 B.empty
  where
    go n acc bs = await >>= \opt ->
      case opt of
        Nothing -> return False
        Just i | (acc+B.length i) > n -> return False
               | (acc+B.length i) < n -> go n (acc+B.length i) (bs <> i)
               | otherwise ->
                  if challenge == (bs <> i)
                    then maybe (return True) (const $ return False) =<< await
                    else return False

accepted :: Handler a
accepted = sendResponseStatus status202 ("Accepted" :: Text)

verificationFailed :: Sub -> Text -> Handler ()
verificationFailed sub mode =
  let topic  = subTopic sub
      params = [("hub.mode", mode)
               ,("hub.topic", topic)
               ,("hub.reason", "Verification failed")] in
  do manager <- fmap httpManager getYesod
     req     <- parseUrl $ show $ subCallbackUrl params sub
     resp    <- http req manager
     responseBody resp $$+- return ()

validateLease :: Maybe Text -> Validation Text (Maybe Int)
validateLease = traverse (number "hub.lease_seconds")

validateUrl :: Text -> Text -> Validation Text Text
validateUrl key input =
  either (Failure . nel . prepend . fromString . show)
           (Success . const input)
           (parse parser "" input *> pure ())
  where
    parser = do
      string "http" <?> "http/https protocol"
      string "s://" <|> string "://"
      some (alphaNum <|> oneOf "-_?/&.:") <?> "no strange symbol in a url"
      eof
    prepend s = key <> ": " <> s
