{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, ImpredicativeTypes #-}

module Main where

import Subscription
import Subscription.Conf
import Subscription.Params
import Subscription.Util
import qualified Subscription.Verification as V

import Control.Monad.Trans.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Error.Class
import Control.Monad.Reader
import Control.Monad.RWS.Lazy

import qualified Data.ByteString as B 
import qualified Data.ByteString.Lazy as L
import Data.Conduit (Conduit(..), ResourceT(..))
import Data.List
import Data.Foldable hiding (find)
import Data.Maybe
import Data.Machine hiding (run)

import Database.MongoDB hiding (Pipe(..), find)

import Network.Wai
import Network.Wai.Handler.Warp hiding (HostPreference(..))
import Network.HTTP.Types
import Network.HTTP.Types.Header

import qualified Data.ByteString.Lazy.Char8 as C

conf :: Conf 
conf = Conf (DB (Host "127.0.0.1" defaultPort) "hub_deiko")

application = server <~ webapp <~ pubSub

main = runMachineT application

server :: MonadIO m => ProcessT m Application a
server = construct $ liftIO . run 8080 =<< await

webapp :: Process (ProcessT (RWST Conf () Report (ResourceT IO)) Request ()) Application
webapp = construct $ await >>= go
  where
    go app = yield $ builder app
    builder app req = let action = runMachineT (app <~ (construct $ yield req))
                          result = runRWST action conf (Report Nothing Sync)
                      in do
                        (_, report, _) <- result -- TODO: use report
                        return (responseLBS status200 [] "todo")

pubSub :: (MonadIO m, MonadState Report m, MonadReader Conf m, MonadError e m)
          => Source (ProcessT m Request ())
pubSub = construct $ yield (subscription <~ (construct $ await >>= go))
  where
    go req = let method  = asRight $ parseMethod $ requestMethod req
                 params  = queryString req
                 path    = pathInfo req
             in case (method, path) of
               (POST, ["subscribe"]) -> traverse_ yield params
               _ -> reportError ClientSide "unhandled request"

isFormUrlEncoded :: Request -> Bool
isFormUrlEncoded = maybe False (const True) . find (== (hContentType, "application/x-www-form-urlencoded")) . requestHeaders

asRight (Right a) = a