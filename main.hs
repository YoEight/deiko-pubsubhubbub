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
import Data.Conduit (Conduit(..))
import Data.List
import Data.Maybe

import Database.MongoDB hiding (find)

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types

import qualified Data.ByteString.Lazy.Char8 as C

conf :: Conf 
conf = Conf (DB (Host "127.0.0.1" defaultPort) "hub_deiko")

main = run 8080 (go handler)
  where
    go app req = do
      r <- lift $ app (asRight $ parseMethod $ requestMethod req) req
      case r of
        Left _ -> return $ responseLBS
                     status404
                     []
                     L.empty

        Right r -> return $ responseLBS
                      status200
                      []
                      $ C.pack (show r)

handler :: StdMethod -> Request -> IO (Either () V.Response) -- doesn't compile and ugly
handler POST req
  | isFormUrlEncoded req = let action = runPipe $ subscription (queryString req)
                           in do
                              response <- runReaderT action conf
                              print response
                              return $ Right response
  | otherwise = return $ Left ()
handler _ _ = return $ Left ()

isFormUrlEncoded :: Request -> Bool
isFormUrlEncoded = maybe False (const True) . find (== ("Content-Type", C.pack "application/x-www-form-urlencoded")) . requestHeaders

asRight (Right a) = a