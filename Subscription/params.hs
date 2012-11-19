module Subscription.Params where

import qualified Data.ByteString as B
import qualified Data.Text as T

data Guilt = ServerSide | ClientSide

data Report = Report (Maybe (Guilt, String)) Strategy

data Strategy = Sync | Async deriving Show

data ReqType = Subscribe | Unsubscribe deriving Show

data Req = Req { reqCallback :: ReqParam
               , reqMode :: ReqParam
               , reqTopic :: ReqParam
               , reqVerify :: ReqParam
               , reqOptionals :: [ReqParam] } deriving Show

data ReqParam = Callback B.ByteString
                | Mode ReqType
                | Topic B.ByteString
                | LeaseSeconds Int
                | Secret T.Text
                | VerifyToken T.Text
                | Verify Strategy deriving Show

isSyncMode :: Req -> Bool
isSyncMode (Req _ _ _ (Verify Sync) _) = True
isSyncMode _ = False

isSubscription :: Req -> Bool
isSubscription (Req _ (Mode Subscribe) _ _ _) = True
isSubscription _ = False