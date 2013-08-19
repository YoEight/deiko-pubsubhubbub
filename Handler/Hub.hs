{-# LANGUAGE OverloadedStrings #-}
module Handler.Hub where

import Import

import Handler.Hub.Subscription (subscribe, unsubscribe)

postHubR :: Handler Text
postHubR = do
  modeOpt <- lookupPostParam "hub.mode"
  maybe (invalidArgs ["hub.mode has not be provided"]) decision modeOpt

decision :: Text -> Handler Text
decision "subscribe"   = subscribe
decision "unsubscribe" = unsubscribe
decision "publish"     = error "todo"
decision x             = invalidArgs ["unknown " <> x <> " for hub.mode"]
