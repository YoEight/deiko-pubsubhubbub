{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Trans

import qualified Data.Text.Lazy      as T

import           Web.Scotty

main = scotty 5000 $ do
         get "/publisher" process

process :: ActionM ()
process =
  do  header "Content-Type" "application/atom+xml" 
      file "feed_test.xml"
