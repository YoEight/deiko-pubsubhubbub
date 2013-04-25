{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Deiko.Hub.Types where

import           Prelude              hiding (lookup)

import           Control.Monad
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bson
import           Data.Bson.Binary
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable        (foldMap)
import           Data.Hashable
import           Data.Int
import qualified Data.Text            as S
import           Data.Text.Encoding   (encodeUtf8)
import qualified Data.Text.Lazy       as T
import           Data.Time.Clock

data SubState = Verified
              | NotVerified
              | Delete deriving Show

data SubParams = SubParams { subCallback     :: S.Text
                           , subMode         :: S.Text
                           , subTopic        :: S.Text
                           , subVerify       :: [S.Text]
                           , subLeaseSeconds :: Maybe Int
                           , subSecret       :: Maybe S.Text
                           , subVerifyToken  :: Maybe S.Text } deriving Show

data Subscription = Sub { subVersion :: Int
                        , subState   :: SubState
                        , subParams  :: SubParams
                        , subDate    :: UTCTime } deriving Show

data HubEvent = Publish
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
    toByteString = BL.toStrict . runPut . putDocument . toBson

instance ToBson SubParams where
    toBson (SubParams callback mode topic verify leaseSeconds secret verifyToken) =
        merge base $ merge lease $ merge sec tok
            where
              base = ["hub.callback" =: callback
                     ,"hub.mode"     =: mode
                     ,"hub.topic"    =: topic
                     ,"hub.verify"   =: verify]

              lease  = foldMap (\i -> ["hub.lease_seconds" =: i]) leaseSeconds
              sec    = foldMap (\s -> ["hub.secret" =: s]) secret
              tok    = foldMap (\t -> ["hub.verify_token" =: t]) verifyToken

instance ToBson Subscription where
    toBson (Sub version state params date) =
         ["state"   =: stateValue state
         ,"version" =: version
         ,"date"    =: date
         ,"params"  =: toBson params]
        where
          stateValue :: SubState -> S.Text
          stateValue Verified    = "verified"
          stateValue NotVerified = "not_verified"
          stateValue Delete      = "delete"

instance Hashable Subscription where
    hashWithSalt salt sub =
        let callback = subCallback $ subParams $ sub
            topic    = subTopic $ subParams $ sub
        in hashWithSalt salt (callback, topic)

instance FromBson a => FromByteString a where
    fromByteString = fromBson . runGet getDocument . BL.fromChunks . return

instance FromBson SubParams where
    fromBson doc = do
      callback <- lookup "hub.callback" doc
      mode     <- lookup "hub.mode" doc
      topic    <- lookup "hub.topic" doc
      verify   <- lookup "hub.verify" doc
      return $ SubParams callback mode topic verify (leaseSeconds doc) (secret doc) (verifyToken doc)
          where
            leaseSeconds :: Document -> Maybe Int
            leaseSeconds = lookup "hub.lease_seconds"

            secret, verifyToken :: Document -> Maybe S.Text
            secret = lookup "hub.secret"
            verifyToken = lookup "hub.verify_token"

instance FromBson Subscription where
    fromBson doc = do state   <- getState doc
                      version <- lookup "version" doc
                      date    <- lookup "date" doc
                      reqDoc  <- lookup "params" doc
                      params  <- fromBson reqDoc
                      return $ Sub version state params date
        where
          getState = liftM go . lookup "state"

          go :: S.Text -> SubState
          go "verified"     = Verified
          go "not_verified" = NotVerified
          go "delete"       = Delete
