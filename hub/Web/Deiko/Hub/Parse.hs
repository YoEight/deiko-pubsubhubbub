{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Web.Deiko.Hub.Parse (parseSubParams, validateUrl) where

import           Prelude                                  hiding (log)

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.RWS
import           Data.Functor.Identity

import           Web.Deiko.Hub.Types

import qualified Data.ByteString.Lazy                     as B
import           Data.Foldable
import           Data.Monoid
import qualified Data.Sequence                            as SS
import           Data.String
import qualified Data.Text                                as S
import qualified Data.Text.Lazy                           as T

import           Text.Parsec.ByteString
import           Text.Parsec.Prim                         hiding ((<|>))
import           Text.Parsec.Text
import           Text.Parsec.Text.Lazy
import           Text.ParserCombinators.Parsec.Char
import           Text.ParserCombinators.Parsec.Combinator
import           Text.ParserCombinators.Parsec.Prim       hiding (label, (<|>))

data ReqState = RS { srsCallback     :: Maybe S.Text
                   , srsMode         :: Maybe S.Text
                   , srsTopic        :: Maybe S.Text
                   , srsVerify       :: Maybe [S.Text]
                   , srsLeaseSeconds :: Maybe Int
                   , srsSecret       :: Maybe S.Text
                   , srsVerifyToken  :: Maybe S.Text }

newtype Chariot = Chariot { unChariot :: S.Text }

newtype ParamParser a = ParamParser { runParamParser :: RWS () Chariot ReqState a }
    deriving (Monad, MonadState ReqState, MonadWriter Chariot, Functor, Applicative)

instance Monoid Chariot where
    mempty  = Chariot mempty
    mappend (Chariot l) (Chariot r)
      | S.null l && S.null r = Chariot mempty
      | S.null l             = Chariot r
      | S.null r             = Chariot l
      | otherwise            = Chariot $ S.append l $ S.append "\n" r

setCallback, setMode, setTopic, setSecret, setVerifyToken, addVerifyParam, log :: S.Text -> ParamParser ()

setCallback t = modify $ \s -> s{srsCallback=Just t}

setMode t = modify $ \s -> s{srsMode=Just t}

setTopic t = modify $ \s -> s{srsTopic=Just t}

setSecret t = modify $ \s -> s{srsSecret=Just t}

setVerifyToken t = modify $ \s -> s{srsVerifyToken=Just t}

setLeaseSeconds :: Int -> ParamParser ()
setLeaseSeconds i = modify $ \s -> s{srsLeaseSeconds=Just i}

addVerifyParam param = modify go
    where
      go s@(RS{srsVerify=Nothing}) = s{srsVerify=Just [param]}
      go s@(RS{srsVerify=Just xs}) = s{srsVerify=Just $ mconcat [xs, [param]]}

log = tell . Chariot

evalParamParser :: (S.Text -> S.Text -> S.Text -> [S.Text] -> Maybe Int -> Maybe S.Text -> Maybe S.Text -> a -> b)
                -> ParamParser a
                -> ReqState
                -> Either S.Text b
evalParamParser k (ParamParser p) s =
    case runRWS p () s of
      (a, (RS callback mode topic verify leaseSeconds secret verifyToken), (Chariot logs))
          | S.null logs -> maybe (Left "mandatory paramaters are missing")  Right (k <$> callback <*> mode <*> topic <*> verify <*> pure leaseSeconds <*> pure secret <*> pure verifyToken <*> pure a)
          | otherwise  -> Left logs

parseSubParams :: MonadError HubError m => [(S.Text, S.Text)] -> m SubParams
parseSubParams xs =
    either (throwError . ParseError . T.fromStrict) return $ evalParamParser mkHubRequest (traverse_ go xs) init_state
        where
          init_state = RS Nothing Nothing Nothing Nothing Nothing Nothing Nothing

          mkHubRequest callback mode topic verify leaseSeconds secret verifyToken _ =
              SubParams callback mode topic verify leaseSeconds secret verifyToken

          go ("hub.callback", url)        = maybe (setCallback url) (log . S.append "error on hub.callback parameter") (validateUrl url)
          go ("hub.mode", mode)           = setMode mode
          go ("hub.topic",url)            = maybe (setTopic url) (log . S.append "error on hub.topic parameter") (validateUrl url)
          go ("hub.verify", verify)       = addVerifyParam verify
          go ("hub.lease_seconds", lease) = maybe (return ()) setLeaseSeconds (validateLeaseSeconds lease)
          go ("hub.secret", secret)       = setSecret secret
          go ("hub.verify_token", token)  = setVerifyToken token
          go param                        = return ()

validateUrl :: (Stream s Identity Char, IsString s) => s -> Maybe s
validateUrl input = either (Just . fromString . show) (const Nothing) (parse parser "" input *> pure ())
  where
    parser = do
      string "http" <?> "http/https protocol"
      string "s://" <|> string "://"
      some (alphaNum <|> oneOf "-_?/&.:") <?> "no strange symbol in a url"
      eof

validateLeaseSeconds :: (Stream s Identity Char, IsString s) => s -> Maybe Int
validateLeaseSeconds input = either (const Nothing) (Just . read) (parse parser "" input)
    where
      parser = do digits <- some digit
                  eof
                  return digits
