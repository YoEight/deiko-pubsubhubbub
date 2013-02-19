{-# LANGUAGE OverloadedStrings #-}
module PubSubHubBub.Types (ParamLit(..)
                          ,SubReq
                          ,Verif
                          ,Notif
                          ,isBytes
                          ,isInt
                          ,isList
                          ,subReqCallback
                          ,subReqMode
                          ,subReqTopic
                          ,subReqVerify
                          ,subReqLeaseSeconds
                          ,subReqVerifyToken
                          ,verifMode
                          ,verifTopic
                          ,verifChallenge
                          ,verifLeaseSeconds
                          ,verifVerifyToken
                          ,contentNotifMode
                          ,contentNotifUrl
                          ,validateSubReqParams
                          ,parseInt
                          ,validateUrl
                          ,hub_callback
                          ,hub_mode
                          ,hub_verify
                          ,hub_lease_seconds
                          ,hub_secret
                          ,hub_verify_token
                          ,hub_url
                          ,hub_challenge
                          ,validateVerifParams
                          ,validateNotifParams) where

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

newtype SubReq = SubReq { subParamMap :: M.Map B.ByteString ParamLit }
newtype Verif = Verif { verifParamMap :: M.Map B.ByteString ParamLit }
newtype Notif = Notif { contentParamMap :: M.Map B.ByteString ParamLit }

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
subReqCallback = (M.! hub_callback) . subParamMap

subReqMode :: SubReq -> ParamLit
subReqMode = (M.! hub_mode) . subParamMap

subReqTopic :: SubReq -> ParamLit
subReqTopic = (M.! hub_topic) . subParamMap

subReqVerify :: SubReq -> ParamLit
subReqVerify = (M.! hub_verify) . subParamMap

subReqLeaseSeconds :: SubReq -> Maybe ParamLit
subReqLeaseSeconds = (M.lookup hub_lease_seconds) . subParamMap

subReqSecret :: SubReq -> Maybe ParamLit
subReqSecret = (M.lookup hub_secret) . subParamMap

subReqVerifyToken :: SubReq -> Maybe ParamLit
subReqVerifyToken = (M.lookup hub_verify_token) . subParamMap

verifMode :: Verif -> ParamLit
verifMode = (M.! hub_mode) . verifParamMap

verifTopic :: Verif -> ParamLit
verifTopic = (M.! hub_topic) . verifParamMap

verifChallenge :: Verif -> ParamLit
verifChallenge = (M.! hub_challenge) . verifParamMap

verifLeaseSeconds :: Verif -> Maybe ParamLit
verifLeaseSeconds = (M.lookup hub_lease_seconds) . verifParamMap

verifVerifyToken :: Verif -> Maybe ParamLit
verifVerifyToken = (M.lookup hub_verify_token) . verifParamMap

contentNotifMode :: Notif -> ParamLit
contentNotifMode = (M.! hub_mode) . contentParamMap

contentNotifUrl :: Notif -> ParamLit
contentNotifUrl = (M.! hub_url) . contentParamMap

validateSubReqParams :: ReaderT (M.Map B.ByteString ParamLit) (Either String) SubReq
validateSubReqParams = do
  validateUrlParam hub_callback
  validateUrlParam hub_topic
  validateMode
  validateVerify
  validateLeaseSeconds
  validateSecret
  validateVerifyToken
  asks SubReq

validateVerifParams :: ReaderT (M.Map B.ByteString ParamLit) (Either String) Verif
validateVerifParams = do
  validateMode
  validateUrlParam hub_topic
  validateChallenge
  validateLeaseSeconds
  validateVerifyToken
  asks Verif

validateNotifParams :: ReaderT (M.Map B.ByteString ParamLit) (Either String) Notif
validateNotifParams = do
  validateMode
  validateUrlParam hub_url
  asks Notif

validateCallback :: ReaderT (M.Map B.ByteString ParamLit) (Either String) ()
validateCallback = validatePresence hub_callback >>= go
  where
    go (PBytes bytes) = lift $ validateUrl bytes
    go _              = lift $ Left "callback param: invalid format"

validateMode :: ReaderT (M.Map B.ByteString ParamLit) (Either String) ParamLit
validateMode = lift . validateFormat isBytes hub_mode =<< validatePresence hub_mode

validateVerify :: ReaderT (M.Map B.ByteString ParamLit) (Either String) ParamLit
validateVerify = lift . validateFormat (\p -> isBytes p || isList p) hub_verify =<< validatePresence hub_verify

validateLeaseSeconds :: ReaderT (M.Map B.ByteString ParamLit) (Either String) ()
validateLeaseSeconds = validateOptional (validateFormat isInt hub_lease_seconds) hub_lease_seconds

validateSecret :: ReaderT (M.Map B.ByteString ParamLit) (Either String) ()
validateSecret = validateOptional (validateFormat isBytes hub_secret) hub_secret

validateChallenge :: ReaderT (M.Map B.ByteString ParamLit) (Either String) ()
validateChallenge = validatePresence hub_challenge >>= (lift . fmap (const ())  . validateFormat isBytes hub_challenge)

validateUrlParam :: B.ByteString ->  ReaderT (M.Map B.ByteString ParamLit) (Either String) ()
validateUrlParam key = validatePresence key >>= go
  where
    go (PBytes bytes) = lift $ validateUrl bytes
    go _              = lift $ Left $ (show key) ++ " param: invalid format"

validateVerifyToken :: ReaderT (M.Map B.ByteString ParamLit) (Either String) ()
validateVerifyToken = validateOptional (validateFormat isBytes hub_verify_token) hub_verify_token

validateOptional :: (ParamLit -> Either String a)
                 -> B.ByteString
                 -> ReaderT (M.Map B.ByteString ParamLit) (Either String) ()
validateOptional k key = do
  param <- asks (M.lookup key)
  maybe (return ()) (lift . fmap (const ()) . k) param

validateFormat :: (ParamLit -> Bool)
               -> B.ByteString
               -> ParamLit
               -> Either String ParamLit
validateFormat p key param
  | p param   = return param
  | otherwise = Left $ (show key) ++ ": invalid format"

validatePresence :: B.ByteString -> ReaderT (M.Map B.ByteString ParamLit) (Either String) ParamLit
validatePresence key = do
  p <- asks (M.lookup key)
  case p of
    Just param -> return param
    Nothing    -> lift $ Left $ (show key) ++ " param is mandatory"

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

hub_callback :: B.ByteString
hub_callback = "hub.callback"

hub_mode :: B.ByteString
hub_mode = "hub.mode"

hub_topic :: B.ByteString
hub_topic = "hub.topic"

hub_verify :: B.ByteString
hub_verify = "hub.verify"

hub_lease_seconds :: B.ByteString
hub_lease_seconds = "hub.lease_seconds"

hub_secret :: B.ByteString
hub_secret = "hub.secret"

hub_verify_token :: B.ByteString
hub_verify_token = "hub.verify_token"

hub_url :: B.ByteString
hub_url = "hub.url"

hub_challenge :: B.ByteString
hub_challenge = "hub.challenge"
