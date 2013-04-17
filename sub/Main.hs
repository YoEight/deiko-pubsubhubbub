{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Trans

import qualified Data.Text.Lazy as T

import Web.Scotty

main = scotty 4000 $ do 
         get "/callback" process

process :: ActionM ()
process = do
  mode      <- param "hub.mode"
  topic     <- param "hub.topic"
  challenge <- param "hub.challenge"
  release   <- param "hub.lease_seconds"
  liftIO $ print ((mode, topic, challenge, release) :: (T.Text, T.Text, T.Text, T.Text)) 
  text challenge
