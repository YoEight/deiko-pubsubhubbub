{-# LANGUAGE OverloadedStrings #-}
module Handler.Hub where

import Import

getHubR :: Handler Text
getHubR = do
  modeOpt <- lookupGetParam "hub.mode"
  maybe (invalidArgs ["hub.mode has not be provided"]) decision modeOpt

decision :: Text -> Handler Text
decision "subscribe"   = subscription
decision "unsubscribe" = unsubscription
decision "publish"     = publish
decision x             = invalidArgs ["unknown " <> x <> " for hub.mode"]

subscription :: Handler Text
subscription = error "subscription not implemented yet"

unsubscription :: Handler Text
unsubscription = error "unsubscription not implemented yet"

publish :: Handler Text
publish = error "publish not implemented yet"
