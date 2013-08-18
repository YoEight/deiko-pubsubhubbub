{-# LANGUAGE OverloadedStrings #-}
module  Handler.Hub.Subscription
  (
    subscribe,
    unsubscribe
  ) where

import Import

import Handler.Hub.Util

import Control.Applicative (Applicative(..), (<|>), some)
import Control.Monad (join)
import qualified Data.ByteString as B
import Data.Conduit
import Data.Foldable (foldMap)
import Data.List.NonEmpty hiding (nonEmpty, insert)
import Data.Monoid (First(..))
import Data.String (fromString)
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

subscription :: SubFinal a -> Handler Text
subscription k = do
  sub   <- mkRequest
  subId <- runDB $ creationRoutine sub
  case subVerify sub of
    "async" -> processed
    "sync"  -> verification sub >> k subId sub >> processed

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
  callback  <- lookupGetParam "hub.callback"
  topic     <- lookupGetParam "hub.topic"
  verif     <- lookupGetParams "hub.verify"
  leaseOpt  <- lookupGetParam "hub.lease_seconds"
  secret    <- lookupGetParam "hub.secret"
  verifyTok <- lookupGetParam "hub.verify_token"
  action callback topic verif leaseOpt secret verifyTok

  where
    validate cb topic verif lease secret verifyTok =
      Sub <$>
      url "hub.callback" cb <*>
      url "hub.topic" topic <*>
      validateVerify verif  <*>
      validateLease lease   <*>
      pure secret           <*>
      pure verifyTok        <*>
      pure False            <*>
      pure True

    action cb topic verify lease secret verifyTok =
      validation (invalidArgs . toList) return $
      validate cb topic verify lease secret verifyTok

    url key opt = validateUrl key -<< param key opt

unsubscription :: Handler Text
unsubscription = error "unsubscription not implemented yet"

publish :: Handler Text
publish = error "publish not implemented yet"

randomString :: MonadIO m => m Text
randomString = return "challenge_test"

verification :: Sub -> Handler ()
verification sub = do
  challenge   <- randomString
  (Just mode) <- lookupGetParam "hub.mode"
  req         <- parseUrl (show $ url mode challenge)
  join $ withManager $ handleResp req challenge

  where
    url m c =
      subCallback sub <> "?" <> foldl1 (\a b -> a <> "&" <> b) (params m c)

    params m c =
      let lease     = subLeaseSeconds sub
          leaseTxt  = fmap (fromString . show) lease in
      mconcat [["hub.mode=" <> m
               ,"hub.topic=" <> subTopic sub
               ,"hub.challenge=" <> c]
              , [foldMap ("hub.lease_seconds=" <>) leaseTxt]]

    handleResp req challenge manager =
      http req manager >>= \resp ->
        let validStatus s = status200 <= s && s < status300
            sink =
              let f x | x         = return ()
                      | otherwise = verificationFailed in
              fmap f (checkChallengeSink (encodeUtf8 challenge)) in
        case responseStatus resp of
          s | validStatus s -> responseBody resp $$+- sink
            | otherwise     -> return verificationFailed

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

processed :: Handler a
processed = sendResponseStatus status204 ("Processed" :: Text)

verificationFailed :: Handler a
verificationFailed = invalidArgs ["Verification failed"]

validateLease :: Maybe Text -> Validation Text (Maybe Int)
validateLease = traverse (number "hub.lease_seconds")

validateVerify :: [Text] -> Validation Text Text
validateVerify = (go -<<) . nonEmpty "hub.verify"
  where
    go =
      let f x | x == "sync"  = First (Just x)
              | x == "async" = First (Just x)
              | otherwise    = First Nothing
          err = Failure $ nel $ "Unsupported hub.verify values" in
      maybe err Success . getFirst . foldMap f

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
