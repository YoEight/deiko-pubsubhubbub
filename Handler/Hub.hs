{-# LANGUAGE OverloadedStrings #-}
module Handler.Hub where

import Import

import Control.Applicative (Applicative(..))
import Data.List.NonEmpty
import qualified Data.Semigroup as S

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
decision "subscribe"   = subscription
decision "unsubscribe" = unsubscription
decision "publish"     = publish
decision x             = invalidArgs ["unknown " <> x <> " for hub.mode"]

subscription :: Handler Text
subscription = error "todo"

unsubscription :: Handler Text
unsubscription = error "unsubscription not implemented yet"

publish :: Handler Text
publish = error "publish not implemented yet"

param :: Text -> Maybe a -> Validation Text a
param _ (Just a) = Success a
param key _      = Failure (nel $ key <> " is not provided")

nel :: a -> NonEmpty a
nel a = a :| []
