{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE UndecidableInstances   #-}

module Web.Deiko.Hub.Types where

import           Prelude              hiding (lookup)

import           Control.Applicative  (Applicative, (<$>))
import           Control.Monad        (liftM)
import           Control.Monad.Trans  (MonadIO (..))

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
import           Data.Monoid
import qualified Data.Text            as S
import           Data.Text.Encoding   (encodeUtf8)
import qualified Data.Text.Lazy       as T
import           Data.Time.Clock

data Verified = Verified deriving Show
data NotVerified = NotVerified deriving Show

data Deletion a where
    Deletion :: Verification v => v -> Deletion v

data Submitted = Submitted deriving Show
data Fetched = Fetched deriving Show

data SubParams = SubParams { subCallback     :: S.Text
                           , subMode         :: S.Text
                           , subTopic        :: S.Text
                           , subVerify       :: [S.Text]
                           , subLeaseSeconds :: Maybe Int
                           , subSecret       :: Maybe S.Text
                           , subVerifyToken  :: Maybe S.Text } deriving Show

data SubInfos = SubInfos { subVersion :: Int
                         , subParams  :: SubParams
                         , subDate    :: UTCTime } deriving Show

data PubInfos = PubInfos { pubUrl     :: S.Text
                         , pubVersion :: Int
                         , pubDate    :: UTCTime } deriving Show

data Sub a where
    Sub :: Verification v => v -> SubInfos -> Sub v

data Pub a where
    Pub :: Publishing p => p -> PubInfos -> Pub p

data HubError = BadRequest T.Text
              | VerificationFailed
              | ParseError T.Text
              | InternalError T.Text deriving (Eq, Show)

instance Show a => Show (Sub a) where
    show (Sub state infos) = mconcat ["Sub ", show state, show infos]

instance Show a => Show (Pub a) where
    show (Pub state url) = mconcat ["Pub ", show state, show url]

class Verification a
class Publishing a
class Pending a
class Ended a b | b -> a where
  end :: b

class Start v where
  start :: v

instance Verification Verified
instance Verification NotVerified
instance Verification v => Verification (Deletion v)

instance Publishing Submitted
instance Publishing Fetched

instance Pending NotVerified
instance Pending v => Pending (Deletion v)

instance Ended NotVerified Verified where
    end = Verified

instance (Ended a b, Verification b) => Ended (Deletion a) (Deletion b) where
    end = Deletion end

instance Start NotVerified where
    start = NotVerified

instance (Start v, Verification v) => Start (Deletion v) where
    start = Deletion start

class ToValue a where
    toValue :: a -> Value

class FromValue a where
    fromValue :: Monad m => Value -> m a

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
    toBson (SubParams cb mode topic verify leaseSeconds secret verifyToken) =
        merge base $ merge lease $ merge sec tok
          where
            base = ["hub.callback" =: cb
                   ,"hub.mode"     =: mode
                   ,"hub.topic"    =: topic
                   ,"hub.verify"   =: verify]

            lease  = foldMap (\i -> ["hub.lease_seconds" =: i]) leaseSeconds
            sec    = foldMap (\s -> ["hub.secret" =: s]) secret
            tok    = foldMap (\t -> ["hub.verify_token" =: t]) verifyToken

instance ToValue a => ToBson (Sub a) where
    toBson (Sub state (SubInfos version params date)) =
      ["state"   := toValue state
      ,"version" =: version
      ,"date"    =: date
      ,"params"  =: toBson params]

instance (Publishing a, ToValue a) => ToBson (Pub a) where
  toBson (Pub state (PubInfos url version date)) =
    ["state"   := toValue state
    ,"url"     =: url
    ,"version" =: version
    ,"date"    =: date]

instance ToValue Verified where
    toValue _ = String "verified"

instance ToValue NotVerified where
    toValue _ = String "not_verified"

instance ToValue a => ToValue (Deletion a) where
    toValue (Deletion a) = go (toValue a)
      where
        go (String s) = String $ S.append "delete_" s

instance ToValue Submitted where
  toValue _ = String "submitted"

instance ToValue Fetched where
  toValue _ = String "fetched"

instance FromValue Verified where
    fromValue (String "verified") = return Verified
    fromValue _ = fail "can't produce a Verified"

instance FromValue NotVerified where
    fromValue (String "not_verified") = return NotVerified
    fromValue _ = fail "can't produce a NotVerified"

instance (FromValue v, Verification v) => FromValue (Deletion v) where
    fromValue (String x) =
        maybe deletionFailure (liftM Deletion . fromValue . String)
                (S.stripPrefix "delete_" x)
    fromValue _ = deletionFailure

instance FromValue Submitted where
  fromValue (String "submitted") = return Submitted
  fromValue _ = fail "can't produce a Submitted"

instance FromValue Fetched where
  fromValue (String "fetched") = return Fetched
  fromValue _ = fail "can't produce a Fetched"

deletionFailure :: Monad m => m a
deletionFailure = fail "can't produce a Deletion"

instance Hashable (Sub a) where
    hashWithSalt salt (Sub _ infos) =
        let callback = subCallback $ subParams $ infos
            topic    = subTopic $ subParams $ infos
        in hashWithSalt salt (callback, topic)

instance FromBson a => FromByteString a where
    fromByteString = fromBson . runGet getDocument . BL.fromChunks . return

instance FromBson SubParams where
    fromBson doc = do
      callback <- lookup "hub.callback" doc
      mode     <- lookup "hub.mode" doc
      topic    <- lookup "hub.topic" doc
      verify   <- lookup "hub.verify" doc
      return $ SubParams  callback mode topic verify (leaseSeconds doc)
                 (secret doc) (verifyToken doc)
        where
          leaseSeconds :: Document -> Maybe Int
          leaseSeconds = lookup "hub.lease_seconds"

          secret, verifyToken :: Document -> Maybe S.Text
          secret = lookup "hub.secret"
          verifyToken = lookup "hub.verify_token"

instance (FromValue v, Verification v) => FromBson (Sub v) where
    fromBson doc =
      do state   <- look "state" doc >>= fromValue
         version <- lookup "version" doc
         date    <- lookup "date" doc
         reqDoc  <- lookup "params" doc
         params  <- fromBson reqDoc
         return $ Sub state (SubInfos version params date)

instance (FromValue p, Publishing p) => FromBson (Pub p) where
  fromBson doc =
    do state   <- look "state" doc >>= fromValue
       url     <- lookup "url" doc
       version <- lookup "version" doc
       date    <- lookup "date" doc
       return $ Pub state $ PubInfos url version date

subInfos :: Sub v -> SubInfos
subInfos (Sub _ infos) = infos

makeSub :: (MonadIO m, Verification v)
        => v
        -> SubParams
        -> m (Sub v)
makeSub v params = liftM ((Sub v) . (SubInfos 1 params)) currentTime
  where
    currentTime = liftIO getCurrentTime
