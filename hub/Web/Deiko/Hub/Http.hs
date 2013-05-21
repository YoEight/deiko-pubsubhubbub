{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
module Web.Deiko.Hub.Http (verify, fetchContent) where

import Control.Applicative       (Applicative, WrappedMonad (..))
import Control.Monad             (liftM)
import Control.Monad.Trans       (MonadIO (..))

import Data.ByteString           (ByteString)
import Data.Foldable             (traverse_)
import Data.Monoid               ((<>))
import Data.String               (IsString (..))
import Data.Text                 (Text, unpack)
import Data.Text.Encoding        (encodeUtf8)

import Network.Curl              (CurlResponse_ (..), curlGetResponse_)
import Network.HTTP.Types.Status (Status, status204, status400)

import Text.Atom.Feed            (Feed)
import Text.Atom.Feed.Import     (elementFeed)
import Text.XML.Light            (parseXMLDoc)

import Web.Deiko.Hub.Types       (Ended (..), Pending (..), Sub, SubParams (..),
                                  Verification (..), makeSub, subInfos,
                                  subParams)

type CurlResp = IO (CurlResponse_ [(String, String)] ByteString)

verify :: (MonadIO m, Pending v, Ended v w, Verification w)
       => Sub v
       -> m (Status, Maybe (Sub w))
verify = go . subParams . subInfos
  where
    go params = do
      challenge <- randomString
      let encChallenge = encodeUtf8 challenge
          url          = unpack (subParamsQuery params challenge)
      response  <- liftIO $ curl url
      case (respStatus response, respBody response) of
        (status, back)
          | validStatus status && back == encChallenge ->
            liftM ((status204,) . Just) (makeSub end params)
        _ -> return (status400, Nothing)

randomString :: (MonadIO m, IsString s) => m s
randomString = return "test_challenge"

subParamsQuery :: SubParams -> Text -> Text
subParamsQuery (SubParams callback mode topic verify _ _ _) challenge =
  callback <> "?" <> foldl1 (\a b -> a <> "&" <> b) params
    where
      params = ["hub.mode=" <> mode
               ,"hub.topic=" <> topic
               ,"hub.challenge=" <> challenge
               ,"hub.lease_seconds=10"]

fetchContent :: MonadIO m => String -> m (Maybe Feed)
fetchContent url =
  do resp <- liftIO $ curl url
     case (respStatus resp, respBody resp) of
       (status, body)
         | validStatus status -> return (elementFeed =<< parseXMLDoc body)
         | otherwise          -> return Nothing

curl :: String -> CurlResp
curl = flip curlGetResponse_ $ []

validStatus :: Int -> Bool
validStatus status = 200 <= status && status < 300
