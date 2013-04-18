{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Deiko.Hub.Types where

import           Prelude              hiding (lookup)

import           Control.Monad
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bson
import           Data.Bson.Binary
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as S
import           Data.Text.Encoding   (encodeUtf8)
import qualified Data.Text.Lazy       as T
import           Data.Time.Clock

data HubRequest = HubRequest { hubCallback :: S.Text
                             , hubMode     :: S.Text
                             , hubTopic    :: S.Text
                             , hubVerify   :: [S.Text]
                             , hubOthers   :: [(S.Text, S.Text)] } deriving Show

data HubEvent = Subscription { subVersion :: Int
                             , subDate    :: UTCTime
                             , subRequest :: HubRequest }
              | Publish
              | Verify deriving Show

data HubError = BadRequest T.Text
              | VerificationFailed
              | ParseError T.Text
              | InternalError T.Text deriving (Eq, Show)

class ToBson a where
    toBson :: a -> Document

class ToByteString a where
    toByteString :: a -> BS.ByteString

class FromBson a where
    fromBson :: Monad m => Document -> m a

class FromByteString a where
    fromByteString :: Monad m => BS.ByteString -> m a

instance ToBson a => ToByteString a where
    toByteString = toStrict . runPut . putDocument . toBson
        where
          toStrict :: BL.ByteString -> BS.ByteString
          toStrict = BS.concat . BL.toChunks

instance ToBson HubRequest where
    toBson (HubRequest callback mode topic verify others) =
        merge base rest
            where
              base = ["hub.callback" =: callback
                     ,"hub.mode" =: mode
                     ,"hub.topic" =: topic
                     ,"hub.verify" =: verify]

              rest = ["others" =: map go others]
                  where
                    go (label, value) = label =: value

instance ToBson HubEvent where
    toBson (Subscription version date request) =
         ["event.type" =: ("subscription" :: S.Text)
         ,"event.version" =: version
         ,"event.date" =: date
         ,"event.request" =: toBson request]

    toBson Publish =
        ["event.type" =: ("publish" :: S.Text)]

    toBson Verify =
        ["event.type" =: ("verify" :: S.Text)]

instance FromBson a => FromByteString a where
    fromByteString = fromBson . runGet getDocument . BL.fromChunks . return

instance FromBson HubRequest where
    fromBson doc = do
      callback <- lookup "hub.callback" doc
      mode     <- lookup "hub.mode" doc
      topic    <- lookup "hub.topic" doc
      verify   <- lookup "hub.verify" doc
      others   <- return $ maybe [] (map go) (lookup "others" doc)
      return $ HubRequest callback mode topic verify others
          where
            go (name := (String value)) = (name, value)

instance FromBson HubEvent where
    fromBson doc = do
      event_type <- lookup "event.type" doc
      case event_type :: S.Text of
        "subscription" -> do version <- lookup "event.version" doc
                             date    <- lookup "event.date" doc
                             reqDoc  <- lookup "event.request" doc
                             request <- fromBson reqDoc
                             return $ Subscription version date request

        "publish"      -> return Publish
        "verify"       -> return Verify
