module Hub.Params where

import qualified Data.ByteString as B
import qualified Data.Text as T

data Guilt = ServerSide | ClientSide deriving Show

data Report = Report (Maybe (Guilt, String)) Strategy deriving Show

data Strategy = Sync | Async deriving Show

data ReqType = Subscribe | Unsubscribe deriving Show

data SubReq = SubReq { reqCallback :: ReqParam
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

isSyncMode :: SubReq -> Bool
isSyncMode (SubReq _ _ _ (Verify Sync) _) = True
isSyncMode _ = False

isSubscription :: SubReq -> Bool
isSubscription (SubReq _ (Mode Subscribe) _ _ _) = True
isSubscription _ = False