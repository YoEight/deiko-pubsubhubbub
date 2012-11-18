{-# LANGUAGE OverloadedStrings #-}

module Main where

import Subscription
import Subscription.Conf
import qualified Subscription.Verification as V

import Control.Monad.Trans.Class
import Control.Monad.Reader
import Control.Pipe

import qualified Data.ByteString as B 
import qualified Data.ByteString.Lazy as L
import Data.Conduit (Conduit(..), ResourceT(..))
import Data.List
import Data.Maybe

import Database.MongoDB hiding (Pipe(..), find)

import Network.Wai
import Network.Wai.Handler.Warp hiding (HostPreference(..))
import Network.HTTP.Types
import Network.HTTP.Types.Header

import qualified Data.ByteString.Lazy.Char8 as C

conf :: Conf 
conf = Conf (DB (Host "127.0.0.1" defaultPort) "hub_deiko")

main = run 8080 (go handler)
  where
    go app req = let action = runPipe $ app (asRight $ parseMethod $ requestMethod req) req
                 in runReaderT action conf
                      
handler :: StdMethod
           -> Request
           -> Pipe () C (ReaderT Conf (ResourceT IO)) Response
handler POST req
  | isFormUrlEncoded req = let go = do
                                 resp <- await
                                 case resp of                                                       
                                   V.Success -> return (responseLBS status200 [] $ C.pack "success")
                                   V.Pending -> return (responseLBS status200 [] $ C.pack "pending")
                                   V.Failure -> return (responseLBS status404 [] $ C.pack "failure")
                           in go <+< subscription (queryString req)
  | otherwise = return (responseLBS status404 [] "failure")
  

isFormUrlEncoded :: Request -> Bool
isFormUrlEncoded = maybe False (const True) . find (== (hContentType, "application/x-www-form-urlencoded")) . requestHeaders

asRight (Right a) = a