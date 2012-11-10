module Subscription.Params where

import qualified Data.ByteString as B
import qualified Data.Text as T

data Strategy = Sync | Async deriving Show

data ReqType = Subscribe | Unsubscribe deriving Show

data Req = Req [ReqParam] deriving Show

data ReqParam = Callback B.ByteString
                | Mode ReqType
                | Topic B.ByteString
                | LeaseSeconds Int
                | Secret T.Text
                | VerifyToken T.Text
                | Verify Strategy deriving Show