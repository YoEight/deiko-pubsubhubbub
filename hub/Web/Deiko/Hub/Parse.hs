{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Web.Deiko.Hub.Parse (parseHubRequest, validateUrl) where

import           Prelude                                  hiding (log)

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Error.Class
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
import           Text.ParserCombinators.Parsec.Char
import           Text.ParserCombinators.Parsec.Combinator
import           Text.ParserCombinators.Parsec.Prim       hiding (label, (<|>))

type Param = (B.ByteString, B.ByteString)

data ReqState s = RS { srsCallback :: Maybe s
                     , srsMode     :: Maybe s
                     , srsTopic    :: Maybe s
                     , srsVerify   :: Maybe [s]
                     , srsOthers   :: [(s, s)] }

newtype ParamParser s a = ParamParser { runParamParser :: ReqState s -> (a, SS.Seq s, ReqState s)  }

instance Functor (ParamParser s) where
    fmap f (ParamParser k) = ParamParser (go . k)
        where
          go (a, log, state) = (f a, log, state)

instance Applicative (ParamParser s) where
    pure  = return
    (<*>) = ap

instance Monad (ParamParser s) where
    return a = ParamParser $ \s -> (a, SS.empty, s)

    ParamParser k >>= f = ParamParser (go . k)
        where
          go (a, l, s) = case runParamParser (f a) s of
                             (b, l', s') -> (b, mappend l l', s')

setCallback, setMode, setTopic, addVerifyParam, log :: IsString s => s -> ParamParser s ()

setCallback t = ParamParser $ \s -> ((), mempty, s{srsCallback=Just t})

setMode t = ParamParser $ \s -> ((), mempty, s{srsMode=Just t})

setTopic t = ParamParser $ \s -> ((), mempty, s{srsTopic=Just t})

addOptionalParam :: IsString s => (s, s) -> ParamParser s ()
addOptionalParam param = ParamParser go
    where
      go s@(RS{srsOthers=xs}) = ((), mempty, s{srsOthers=param:xs})

addVerifyParam param = ParamParser go
    where
      go s@(RS{srsVerify=Nothing}) = ((), mempty, s{srsVerify=Just [param]})
      go s@(RS{srsVerify=Just xs}) = ((), mempty, s{srsVerify=Just $ mconcat [xs, [param]]})

log l = ParamParser $ \s -> ((), SS.singleton l, s)

evalParamParser :: (IsString s, Monoid s)
                => (s -> s -> s -> [s] -> [(s, s)] -> a -> b)
                -> ParamParser s a
                -> ReqState s
                -> Either s b
evalParamParser k (ParamParser p) s =
    case p s of
      (a, logs, (RS callback mode topic verify others))
          | SS.null logs -> maybe (Left "mandatory paramaters are missing")  Right (k <$> callback <*> mode <*> topic <*> verify <*> pure others <*> pure a)
          | otherwise  -> Left $ foldMap (flip mappend "\n") logs

parseHubRequest :: MonadError HubError m => [(S.Text, S.Text)] -> m HubRequest
parseHubRequest xs =
    either (throwError . ParseError . T.fromChunks . return) return $ evalParamParser mkHubRequest (traverse_ go xs) init_state
        where
          init_state = RS Nothing Nothing Nothing Nothing []

          mkHubRequest callback mode topic verify others _ =
              HubRequest callback mode topic verify others

          go ("hub.callback", url)  = maybe (setCallback url) (log . S.append "error on hub.callback parameter") (validateUrl url)
          go ("hub.mode", mode)     = setMode mode
          go ("hub.topic",url)      = maybe (setTopic url) (log . S.append "error on hub.topic parameter") (validateUrl url)
          go ("hub.verify", verify) = addVerifyParam verify
          go param                  = addOptionalParam param

validateUrl :: (Stream s Identity Char, IsString s) => s -> Maybe s
validateUrl input = either (Just . fromString . show) (const Nothing) (parse parser "" input *> pure ())
  where
    parser = do
      string "http" <?> "http/https protocol"
      string "s://" <|> string "://"
      some (alphaNum <|> oneOf "-_?/&.:") <?> "no strange symbol in a url"
      eof
