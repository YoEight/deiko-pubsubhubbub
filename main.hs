{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, ImpredicativeTypes #-}

module Main where

import Subscription
import Subscription.Conf
import Hub.Params
import Subscription.Util
import qualified Subscription.Verification as V

import Control.Monad.Trans.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Error.Class
import Control.Monad.Reader hiding (mapM_)
import Control.Monad.RWS.Lazy hiding (mapM_)

import qualified Data.ByteString as B 
import qualified Data.ByteString.Lazy as L
import Data.Conduit (Conduit(..), ResourceT(..))
import Data.List
import Data.Foldable hiding (find, mapM_)
import Data.Maybe
import Data.Machine hiding (run)

import Database.MongoDB hiding (Pipe(..), find)

import Network.Wai
import Network.Wai.Parse
import Network.Wai.Handler.Warp hiding (HostPreference(..))
import Network.HTTP.Types
import Network.HTTP.Types.Header

import qualified Data.ByteString.Lazy.Char8 as C
 
conf :: Conf 
conf = Conf (DB (Host "127.0.0.1" defaultPort) "hub_deiko")

application = server <~ pubSub

main = runMachineT application

server :: MonadIO m => ProcessT m Application a
server = construct $ liftIO . run 8080 =<< await

pubSub :: Source Application
pubSub = construct $ yield go
  where
    go = \req -> let method    = asRight $ parseMethod $ requestMethod req
                     paramData = parseRequestBody lbsBackEnd req
                     path      = pathInfo req
                 in case (method, path) of
                   (POST, ["subscribe"])
                     | isFormUrlEncoded req -> let action params = runMachineT (subscription <~ source params)
                                                   result :: [Param] -> ResourceT IO (Step k o (MachineT (RWST Conf () Report (ResourceT IO)) k o), Report, ())
                                                   result params = runRWST (action params) conf (Report Nothing Sync)
                                               in do
                                                 (params, _)     <- paramData
                                                 (_ , report, _) <- result params
                                                 return $ responseLBS status200 [] "todo"
                   _ -> return $ responseLBS status404 [] ""

isFormUrlEncoded :: Request -> Bool
isFormUrlEncoded = maybe False (const True) . find (== (hContentType, "application/x-www-form-urlencoded")) . requestHeaders

asRight (Right a) = a