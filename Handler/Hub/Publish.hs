{-# LANGUAGE OverloadedStrings #-}
module Handler.Hub.Publish (publish) where

import Import
import qualified Data.ByteString as B
import Data.Conduit
import Data.Hashable (hash)
import Data.List.NonEmpty (toList)
import Handler.Hub.Util
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import Control.Concurrent (forkIO)

type PubFinal a = KeyBackend (PersistEntityBackend Pub) Pub
                -> Pub
                -> Handler a

publish :: Handler Text
publish = do
  pub     <- mkPublish
  pubId   <- runDB $ creationRoutine pub
  handler <- handlerToIO
  liftIO $ forkIO $ handler (fetching pubId pub)
  accepted
    where
      creationRoutine pub =
        do pubId <- insert pub
           date  <- liftIO getCurrentTime
           insert_ (PubHist pubId "creation" date)
           return pubId

mkPublish :: Handler Pub
mkPublish = do
  urlOpt <- lookupPostParam "hub.url"
  validation (invalidArgs . toList) return (action urlOpt)
    where
      action urlOpt =
        Pub <$> url "hub.url" urlOpt <*> pure False

accepted :: Handler a
accepted = sendResponseStatus status202 ("Accepted" :: Text)

fetching :: PubFinal ()
fetching pubId pub = do
  manager <- fmap httpManager getYesod
  req     <- parseUrl (show $ pubTopic $ pub)
  resp    <- http req manager
  mbytes  <- responseBody resp $$+- sink
  runDB $ maybe onError dbAction mbytes
    where
      sink =
        let loop 0 acc = return (Just acc)
            loop n acc =
              await >>= \mbs ->
                case mbs of
                  Nothing -> return (Just acc)
                  Just bs | n - (B.length bs) < 0 -> return Nothing
                          | otherwise -> loop (n - (B.length bs)) (acc <> bs) in
        loop 250000 mempty

      dbAction bytes =
        do update pubId [PubFetched =. True]
           date <- liftIO getCurrentTime
           insert_ (PubContent pubId bytes (hash bytes) date)
           insert_ (PubHist pubId "fetched" date)
      onError =
        do date <- liftIO getCurrentTime
           insert_ (PubHist pubId "error" date)
