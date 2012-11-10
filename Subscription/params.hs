module Subscription.Params where

import qualified Data.Text as T

data Strategy = Sync | Async deriving Show

data SubReq = SubReq [SubParam] deriving Show

data SubParam = Callback T.Text
                | Mode T.Text
                | Topic T.Text
                | LeaseSeconds Int
                | Secret T.Text
                | VerifyToken T.Text
                | Verify Strategy deriving Show