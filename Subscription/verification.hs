module Subscription.Verification (verification) where

import Subscription.Params

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Error.Class
import Control.Monad.Trans.Either
import Control.Pipe

import Data.Either

data Response = Success | Failure | Pending

verification :: Pipe (Either String Req) Response IO ()
verification = await >>= go
  where
    go (Left _)    = yield Failure
    go (Right req)
      | isSyncMode req = sync req
      | otherwise      = async req

sync :: Req -> Pipe a Response IO ()
sync req = do
  result <- (lift . runEitherT) $ (confirmation req) >> (save req)
  either (yield . const Failure) (yield . const Success) result 
  
async :: Req -> Pipe a Response IO ()
async req = do
  result <- lift $ (runEitherT . save) req
  either (yield . const Failure) (yield . const Success) result

-- Database call. Will use MongoDB
save :: Req -> EitherT () IO ()
save req = EitherT $ (return $ Right ()) `catchError` (return . Left . const ())

-- Do a GET request using action request callback
confirmation :: Req -> EitherT () IO ()
confirmation req = EitherT $ (return $ Right ()) `catchError` (return . Left . const ())