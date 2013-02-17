module PubSubHubBub.Types (ParamLit(..)
                          ,SubReq
                          ,VerifSubReq
                          ,ContentNotif
                          ,isBytes
                          ,isInt
                          ,isList
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
                          ,validateContentNotif
                          ,validateSubReqParams
                          ,parseInt
                          ,validateUrl) where

import           Control.Applicative
import           Control.Monad.Reader

import           Data.Bifunctor
import qualified Data.ByteString                          as B
import           Data.List.NonEmpty
import qualified Data.Map                                 as M
import           Data.Maybe
import           Data.Semigroup

import           PubSubHubBub.Validate

import           Text.Parsec.ByteString
import           Text.ParserCombinators.Parsec.Char
import           Text.ParserCombinators.Parsec.Combinator
import           Text.ParserCombinators.Parsec.Prim       hiding ((<|>))

data ParamLit = PInt Int
              | PBytes B.ByteString
              | PList [ParamLit]

newtype SubReq = SubReq { subParamMap :: M.Map String ParamLit }
newtype VerifSubReq = VerifSubReq { verifParamMap :: M.Map String ParamLit }
newtype ContentNotif = ContentNotif { contentParamMap :: M.Map String ParamLit }

isBytes :: ParamLit -> Bool
isBytes (PBytes _) = True
isBytes _          = False

isInt :: ParamLit -> Bool
isInt (PInt _) = True
isInt _        = False

isList :: ParamLit -> Bool
isList (PList _) = True
isList _         = False

subReqCallback :: SubReq -> ParamLit
subReqCallback = (M.! "callback") . subParamMap

subReqMode :: SubReq -> ParamLit
subReqMode = (M.! "mode") . subParamMap

subReqTopic :: SubReq -> ParamLit
subReqTopic = (M.! "topic") . subParamMap

subReqVerify :: SubReq -> ParamLit
subReqVerify = (M.! "verify") . subParamMap

subReqLeaseSeconds :: SubReq -> Maybe ParamLit
subReqLeaseSeconds = (M.lookup "lease_seconds") . subParamMap

subReqSecret :: SubReq -> Maybe ParamLit
subReqSecret = (M.lookup "secret") . subParamMap

subReqVerifyToken :: SubReq -> Maybe ParamLit
subReqVerifyToken = (M.lookup "verify_token") . subParamMap

verifSubReqMode :: VerifSubReq -> ParamLit
verifSubReqMode = (M.! "mode") . verifParamMap

verifSubReqTopic :: VerifSubReq -> ParamLit
verifSubReqTopic = (M.! "topic") . verifParamMap

verifSubReqChallenge :: VerifSubReq -> ParamLit
verifSubReqChallenge = (M.! "challenge") . verifParamMap

verifSubReqLeaseSeconds :: VerifSubReq -> Maybe ParamLit
verifSubReqLeaseSeconds = (M.lookup "lease_seconds") . verifParamMap

verifSubReqVerifyToken :: VerifSubReq -> Maybe ParamLit
verifSubReqVerifyToken = (M.lookup "verify_token") . verifParamMap

contentNotifMode :: ContentNotif -> ParamLit
contentNotifMode = (M.! "mode") . contentParamMap

contentNotifUrl :: ContentNotif -> ParamLit
contentNotifUrl = (M.! "url") . contentParamMap

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

validateSubReqParams :: ReaderT (M.Map String ParamLit) (Either String) SubReq
validateSubReqParams = do
  validateCallback
  validateMode
  validateVerify
  validateLeaseSeconds
  validateSecret
  validateVerifyToken
  asks SubReq

validateCallback :: ReaderT (M.Map String ParamLit) (Either String) ()
validateCallback = validatePresence "callback" >>= go
  where
    go (PBytes bytes) = lift $ validateUrl bytes
    go _              = lift $ Left "callback param: invalid format"

validateMode :: ReaderT (M.Map String ParamLit) (Either String) ParamLit
validateMode = lift . validateFormat isBytes "mode" =<< validatePresence "mode"

validateVerify :: ReaderT (M.Map String ParamLit) (Either String) ParamLit
validateVerify = lift . validateFormat (\p -> isBytes p || isList p) "verify" =<< validatePresence "verify"

validateLeaseSeconds :: ReaderT (M.Map String ParamLit) (Either String) ()
validateLeaseSeconds = validateOptional (validateFormat isInt "lease_seconds") "lease_seconds"

validateSecret :: ReaderT (M.Map String ParamLit) (Either String) ()
validateSecret = validateOptional (validateFormat isBytes "secret") "secret"

validateVerifyToken :: ReaderT (M.Map String ParamLit) (Either String) ()
validateVerifyToken = validateOptional (validateFormat isBytes "verify_token") "verify_token"

validateOptional :: (ParamLit -> Either String a)
                 -> String
                 -> ReaderT (M.Map String ParamLit) (Either String) ()
validateOptional k key = do
  param <- asks (M.lookup key)
  maybe (return ()) (lift . fmap (const ()) . k) param

validateFormat :: (ParamLit -> Bool)
               -> String
               -> ParamLit
               -> Either String ParamLit
validateFormat p key param
  | p param   = return param
  | otherwise = Left $ key ++ ": invalid format"

validatePresence :: String -> ReaderT (M.Map String ParamLit) (Either String) ParamLit
validatePresence key = do
  p <- asks (M.lookup key)
  case p of
    Just param -> return param
    Nothing    -> lift $ Left $ key ++ " param is mandatory"

validateUrl :: B.ByteString -> Either String ()
validateUrl input = bimap show (const ()) (parse parser "" input *> pure ())
  where
    parser = do
      string "http"
      string "s://" <|> string "://"
      some (alphaNum <|> oneOf "-_?/&.:") <?> "invalid character in url"
      eof

parseInt :: B.ByteString -> Either String Int
parseInt input = bimap show id (parse parser "" input)
  where  parser = read <$> some digit <* eof
