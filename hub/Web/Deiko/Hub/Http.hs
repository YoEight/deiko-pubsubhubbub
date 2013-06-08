{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
module Web.Deiko.Hub.Http (verify, fetchContent) where

import Control.Applicative       (Applicative, WrappedMonad (..))
import Control.Exception
import Control.Monad             (liftM, (<=<))
import Control.Monad.Trans       (MonadIO (..))

import Data.ByteString           (ByteString)
import Data.Foldable             (traverse_)
import Data.Monoid               ((<>))
import Data.String               (IsString (..))
import Data.Text                 (Text, unpack)
import Data.Text.Encoding        (encodeUtf8)

import Network.HTTP              (Response (..), getRequest,
                                  postRequestWithBody, simpleHTTP)
import Network.HTTP.Types.Status (Status, status204, status400, status500)
import Network.Stream            (Result, failMisc)

import System.IO.Error           (catchIOError)
import System.Log.Logger         (errorM)

import Text.Atom.Feed            (Feed)
import Text.Atom.Feed.Import     (elementFeed)
import Text.XML.Light            (Element, parseXMLDoc, showElement)

import Web.Deiko.Hub.Types       (Ended (..), Pending (..), Sub, SubParams (..),
                                  Verification (..), makeSub, subInfos,
                                  subParams)

verify :: (MonadIO m, Pending v, Ended v w, Verification w)
       => Sub v
       -> m (Status, Maybe (Sub w))
verify = go . subParams . subInfos
  where
    onError _ = (status400, Nothing)

    onIOError url x = do
      errorM "Web.Deiko.Hub.Http.verify" ("Can't reach url: " ++ url)
      return $ failMisc "connection error"

    go params = do
      challenge <- randomString
      sub       <- makeSub end params
      let url      = unpack (subParamsQuery params challenge)
          request  = getRequest url
          handle back
            | back == unpack challenge = (status204, Just sub)
            | otherwise                = (status400, Nothing)
      resp  <- liftIO $ (simpleHTTP request `catchIOError` (onIOError url))
      return $ either onError id ((onValidStatus handle) =<< resp)

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
  do resp <- liftIO $ simpleHTTP (getRequest url)
     let feed =
           onValidStatus (elementFeed <=< parseXMLDoc) =<< resp
     return $ either (const Nothing) id feed

distributeFeed :: MonadIO m => String -> Element -> m ()
distributeFeed url xml =
  let request = postRequestWithBody
                url
                "application/atom+xml"
                (showElement xml)
  in do res <- liftIO $ simpleHTTP request
        liftIO $ either (print . show) (const (return ())) res

onValidStatus :: (String -> a) -> Response String -> Result a
onValidStatus f response =
  case rspCode response of
    (2, _, _) -> return (f $ rspBody response)
    _         -> failMisc "not valid status"
