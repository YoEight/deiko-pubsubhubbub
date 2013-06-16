{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}

module Web.Deiko.Hub.Parse (parseSubParams, validateUrl, MapError(..)) where

import           Control.Applicative                      hiding (optional)
import           Data.Functor.Identity

import           Web.Deiko.Hub.Types

import           Data.Foldable
import qualified Data.Map                                 as M
import           Data.String
import qualified Data.Text                                as S
import qualified Data.Text.Lazy                           as T

import           Text.Parsec.Prim                         hiding ((<|>))
import           Text.Parsec.Text
import           Text.Parsec.Text.Lazy
import           Text.ParserCombinators.Parsec.Char
import           Text.ParserCombinators.Parsec.Combinator hiding (optional)
import           Text.ParserCombinators.Parsec.Prim       hiding (label, (<|>))

data Param = Single S.Text
           | Multiple [S.Text]

data MapError = MError { errKey :: S.Text
                       , errMsg :: S.Text }

newtype Mapping a =
  Mapping { runMapping :: M.Map S.Text Param -> Either [MapError] a }

instance Functor Mapping where
  fmap f (Mapping k) = Mapping $ \m ->
                         either Left (Right . f) (k m)

instance Applicative Mapping where
  pure a = Mapping (\_ -> Right a)

  Mapping kf <*> Mapping ka =
    Mapping $ \m ->
      case (kf m, ka m) of
        (Left xs, Left vs) -> Left (xs ++ vs)
        (Right f, Right a) -> Right (f a)
        (Left xs, _)       -> Left xs
        (_, Left xs)       -> Left xs

parseSubParams :: [(S.Text, S.Text)] -> Either [MapError] SubParams
parseSubParams = runMapping subParamsMapping . combineParams

subParamsMapping :: Mapping SubParams
subParamsMapping =
  SubParams <$>
  url "hub.callback" <*>
  text "hub.mode" <*>
  url "hub.topic" <*>
  texts "hub.verify" <*>
  optional (integer "hub.lease_seconds") <*>
  optional (text "hub.secret") <*>
  optional (text "hub.verify_token")

keyof :: S.Text
      -> (S.Text -> M.Map S.Text Param -> Either [MapError] a)
      -> Mapping a
keyof key k = Mapping (k key)

optional :: Mapping a -> Mapping (Maybe a)
optional (Mapping k) =
  Mapping $ \m ->
    either (\_ -> Right Nothing) (Right . Just) (k m)

text :: S.Text -> Mapping S.Text
text key = keyof key text_k

texts :: S.Text -> Mapping [S.Text]
texts key = keyof key (presence_k (multiple_k go))
  where
    go _ xs = Right xs

url :: S.Text -> Mapping S.Text
url key = keyof key url_k

integer :: S.Text -> Mapping Integer
integer key = keyof key integer_k

text_k :: S.Text
       -> M.Map S.Text Param
       -> Either [MapError] S.Text
text_k =
  presence_k (single_k go)
  where
    go _ x = Right x

url_k :: S.Text
      -> M.Map S.Text Param
      -> Either [MapError] S.Text
url_k =
  presence_k (single_k go)
  where
    go key x = maybe (Right x) (mapError key) (validateUrl x)

integer_k :: S.Text
          -> M.Map S.Text Param
          -> Either [MapError] Integer
integer_k =
  presence_k (single_k go)
  where
    go key x =
      either (mapError key) Right (validateInt x)

presence_k :: (S.Text -> Param -> Either [MapError] a)
           -> S.Text
           -> M.Map S.Text Param
           -> Either [MapError] a
presence_k k key m =
  maybe (presence_msg key) (k key) (M.lookup key m)

single_k :: (S.Text -> S.Text -> Either [MapError] a)
         -> S.Text
         -> Param
         -> Either [MapError] a
single_k k key (Single x) = k key x
single_k k key (Multiple _) =
  mapError key "is multiple. expected a single value"

multiple_k :: (S.Text -> [S.Text] -> Either [MapError] a)
           -> S.Text
           -> Param
           -> Either [MapError] a
multiple_k k key (Single x)    = k key [x]
multiple_k k key (Multiple xs) = k key xs

presence_msg :: S.Text -> Either [MapError] b
presence_msg key = mapError key "is not present"

mapError :: S.Text -> S.Text -> Either [MapError] b
mapError key msg = Left [MError key msg]

combineParams :: [(S.Text, S.Text)] -> M.Map S.Text Param
combineParams xs =
  let toParam (k, v) = (k, Single v)
      combine (Single x) (Single y)    = Multiple [x, y]
      combine (Single x) (Multiple xs) = Multiple (x:xs) in
  M.fromListWith combine (fmap toParam xs)

validateUrl :: (Stream s Identity Char, IsString s, IsString ss)
            => s
            -> Maybe ss
validateUrl input =
  either (Just . fromString . show)
           (const Nothing)
           (parse parser "" input *> pure ())
  where
    parser = do
      string "http" <?> "http/https protocol"
      string "s://" <|> string "://"
      some (alphaNum <|> oneOf "-_?/&.:") <?> "no strange symbol in a url"
      eof

validateInt :: (Stream s Identity Char, IsString s, IsString ss)
            => s
            -> Either ss Integer
validateInt input =
  either (Left . fromString . show)
         (Right . read)
         (parse go "" input)
  where
    go = do x  <- char '0' <|> digit
            xs <- many1 (oneOf "123456789") <|> pure []
            eof
            return (x:xs)


validateLeaseSeconds :: S.Text -> Maybe Int
validateLeaseSeconds input = either (const Nothing) (Just . read) (parse parser "" input)
    where
      parser = do digits <- some digit
                  eof
                  return digits
