{-# LANGUAGE OverloadedStrings #-}
module Handler.Hub where

import Import

import Control.Applicative (Applicative(..), (<$>), (<|>), some)
import Data.Char (isDigit)
import Data.Foldable (foldMap)
import Data.List.NonEmpty hiding (nonEmpty)
import Data.Monoid (First(..))
import qualified Data.Semigroup as S
import Data.String (fromString)
import Data.Traversable (traverse)
import Text.Parsec.Text
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator hiding (optional)
import Text.ParserCombinators.Parsec.Prim hiding (label, (<|>))

infixl 1 >>-
infixl 1 -<<

data Validation e a = Failure (NonEmpty e)
                    | Success a

instance Functor (Validation e) where
  fmap f (Success a) = Success (f a)
  fmap _ (Failure e) = Failure e

instance Applicative (Validation e) where
  pure = Success
  Success f <*> Success a  = Success (f a)
  Failure e <*> Failure e' = Failure (e S.<> e')
  Failure e <*> _          = Failure e
  _         <*> Failure e  = Failure e

getHubR :: Handler Text
getHubR = do
  modeOpt <- lookupGetParam "hub.mode"
  maybe (invalidArgs ["hub.mode has not be provided"]) decision modeOpt

decision :: Text -> Handler Text
decision "subscribe"   = subscription (error "todo")
decision "unsubscribe" = unsubscription
decision "publish"     = publish
decision x             = invalidArgs ["unknown " <> x <> " for hub.mode"]

subscription :: (Sub -> Handler Text) -> Handler Text
subscription k = do
  sub <- mkRequest
  error "todo"

mkRequest :: Handler Sub
mkRequest = do
  callback  <- lookupGetParam "hub.callback"
  topic     <- lookupGetParam "hub.topic"
  verif     <- lookupGetParams "hub.verify"
  leaseOpt  <- lookupGetParam "hub.lease_seconds"
  secret    <- lookupGetParam "hub.secret"
  verifyTok <- lookupGetParam "hub.verify_token"
  action callback topic verif leaseOpt secret verifyTok
  
  where
    validate cb topic verif lease secret verifyTok =
      Sub <$>
      url "hub.callback" cb <*>
      url "hub.topic" topic <*>
      validateVerify verif  <*>
      validateLease lease   <*>
      pure secret           <*>
      pure verifyTok        <*>
      pure False

    action cb topic verify lease secret verifyTok =
      validation (invalidArgs . toList) return $
      validate cb topic verify lease secret verifyTok

    url key opt = validateUrl key -<< param key opt

unsubscription :: Handler Text
unsubscription = error "unsubscription not implemented yet"

publish :: Handler Text
publish = error "publish not implemented yet"

param :: Text -> Maybe a -> Validation Text a
param _ (Just a) = Success a
param key _      = Failure (nel $ key <> " is not provided")

nonEmpty :: Text -> [Text] -> Validation Text (NonEmpty Text)
nonEmpty _ (x:xs) = Success (x :| xs)
nonEmpty key _    = Failure (nel $ key <> " is not provided")

number :: Text -> Text -> Validation Text Int
number key value = 
  case show value of
    []                  -> notNumber []
    xs | all isDigit xs -> Success (read xs)
       | otherwise      -> notNumber xs
  where
    notNumber str = Failure (nel $ key <> " is not a member")

validateLease :: Maybe Text -> Validation Text (Maybe Int)
validateLease = traverse (number "hub.lease_seconds")

(>>-) :: Validation e a -> (a -> Validation e b) -> Validation e b
Failure e >>- _ = Failure e
Success a >>- f = f a

(-<<) :: (a -> Validation e b) -> Validation e a -> Validation e b
f -<< m = m >>- f

validateVerify :: [Text] -> Validation Text Text
validateVerify = (go -<<) . nonEmpty "hub.verify"
  where
    go =
      let f x | x == "sync"  = First (Just x)
              | x == "async" = First (Just x)
              | otherwise    = First Nothing
          err = Failure $ nel $ "Unsupported hub.verify values" in
      maybe err Success . getFirst . foldMap f

validateUrl :: Text -> Text -> Validation Text Text
validateUrl key input =
  either (Failure . nel . prepend . fromString . show)
           (Success . const input)
           (parse parser "" input *> pure ())
  where
    parser = do
      string "http" <?> "http/https protocol"
      string "s://" <|> string "://"
      some (alphaNum <|> oneOf "-_?/&.:") <?> "no strange symbol in a url"
      eof
    prepend s = key <> ": " <> s

validation :: (NonEmpty e -> x) -> (a -> x) -> Validation e a -> x
validation err _ (Failure e) = err e
validation _ k   (Success a) = k a

nel :: a -> NonEmpty a
nel a = a :| []
