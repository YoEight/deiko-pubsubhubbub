module PubSubHubBub.Types (ParamLit
                          ,SubReq
                          ,VerifSubReq
                          ,ContentNotif
                          ,litInt
                          ,litBytes
                          ,litText
                          ,litList
                          ,subReqCallback
                          ,subReqMode
                          ,subReqTopic
                          ,subReqVerify
                          ,subReqLeaseSeconds
                          ,subReqVerifyToken
                          ,verifSubReqMode
                          ,verifSubReqTopic
                          ,verifSubReqChallenge
                          ,verifSubReqLeaseSeconds
                          ,verifSubReqVerifyToken
                          ,contentNotifMode
                          ,contentNotifUrl
                          ,validateSubReq
                          ,validateVerifSubReq
                          ,validateContentNotif) where

import           Control.Applicative
import qualified Data.ByteString       as B
import           Data.List.NonEmpty
import qualified Data.Map              as M
import           Data.Maybe
import           Data.Semigroup
import qualified Data.Text             as T
import           PubSubHubBub.Validate

data ParamLit = PInt Int
              | PBytes B.ByteString 
              | PText T.Text
              | PList [ParamLit]

newtype SubReq = SubReq { subParamMap :: M.Map String ParamLit }
newtype VerifSubReq = VerifSubReq { verifParamMap :: M.Map String ParamLit }
newtype ContentNotif = ContentNotif { contentParamMap :: M.Map String ParamLit }

litInt :: Int -> ParamLit
litInt = PInt

litBytes :: B.ByteString -> ParamLit
litBytes = PBytes

litText :: T.Text -> ParamLit
litText = PText

litList :: [ParamLit] -> ParamLit
litList = PList

getInt :: ParamLit -> Int
getInt (PInt i) = i

getText :: ParamLit -> T.Text
getText (PText t) = t

getBytes :: ParamLit -> B.ByteString
getBytes (PBytes b) = b

getList :: ParamLit -> [ParamLit]
getList (PList xs) = xs

subReqCallback :: SubReq -> B.ByteString
subReqCallback = getBytes . (M.! "callback") . subParamMap

subReqMode :: SubReq -> B.ByteString
subReqMode = getBytes . (M.! "mode") . subParamMap

subReqTopic :: SubReq -> B.ByteString
subReqTopic = getBytes . (M.! "topic") . subParamMap

subReqVerify :: SubReq -> NonEmpty B.ByteString
subReqVerify = go . fmap getBytes . getList . (M.! "verify") . subParamMap
  where
    go (x:xs) = x :| xs

subReqLeaseSeconds :: SubReq -> Maybe Int
subReqLeaseSeconds = fmap getInt . (M.lookup "lease_seconds") . subParamMap

subReqSecret :: SubReq -> Maybe T.Text
subReqSecret = fmap getText . (M.lookup "secret") . subParamMap

subReqVerifyToken :: SubReq -> Maybe T.Text
subReqVerifyToken = fmap getText . (M.lookup "verify_token") . subParamMap

verifSubReqMode :: VerifSubReq -> B.ByteString
verifSubReqMode = getBytes . (M.! "mode") . verifParamMap

verifSubReqTopic :: VerifSubReq -> B.ByteString
verifSubReqTopic = getBytes . (M.! "topic") . verifParamMap

verifSubReqChallenge :: VerifSubReq -> B.ByteString
verifSubReqChallenge = getBytes . (M.! "challenge") . verifParamMap

verifSubReqLeaseSeconds :: VerifSubReq -> Maybe Int
verifSubReqLeaseSeconds = fmap getInt . (M.lookup "lease_seconds") . verifParamMap

verifSubReqVerifyToken :: VerifSubReq -> Maybe T.Text
verifSubReqVerifyToken = fmap getText . (M.lookup "verify_token") . verifParamMap

contentNotifMode :: ContentNotif -> B.ByteString
contentNotifMode = getBytes . (M.! "mode") . contentParamMap

contentNotifUrl :: ContentNotif -> B.ByteString
contentNotifUrl = getBytes . (M.! "url") . contentParamMap

validateSubReq :: M.Map String ParamLit -> ValidateNEL String SubReq
validateSubReq m =
  (\_ _ _ _ -> SubReq m)
  <$> isPresent "callback"
  <*> isPresent "mode"
  <*> isPresent "topic"
  <*> isPresent "verify"
    where
      isPresent param = validateParam (M.lookup param m) param

validateVerifSubReq :: M.Map String ParamLit -> ValidateNEL String VerifSubReq
validateVerifSubReq m =
  (\_ _ _ -> VerifSubReq m)
  <$> isPresent "mode"
  <*> isPresent "topic"
  <*> isPresent "challenge"
    where
      isPresent param = validateParam (M.lookup param m) param

validateContentNotif :: M.Map String ParamLit -> ValidateNEL String ContentNotif
validateContentNotif m =
  (\_ _-> ContentNotif m)
  <$> isPresent "mode"
  <*> isPresent "topic"
    where
      isPresent param = validateParam (M.lookup param m) param

validateParam :: Maybe ParamLit -> String -> ValidateNEL String ParamLit
validateParam Nothing msg  = failure (("Parameter is missing:" ++ msg) :| [])
validateParam (Just lit) _ = success lit
