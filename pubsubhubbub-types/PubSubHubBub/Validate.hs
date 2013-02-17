module PubSubHubBub.Validate where

import Control.Applicative
import Data.List.NonEmpty
import Data.Semigroup

type ValidateNEL e a = Validate (NonEmpty e) a

data Validate e a = Failure e
                  | Success a

instance Functor (Validate e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Semigroup e => Applicative (Validate e) where
  pure = Success
  Success f <*> Success a  = Success (f a)
  Failure e <*> Failure e' = Failure (e <> e')
  Failure e <*> _          = Failure e
  _         <*> Failure e  = Failure e

failure :: e -> Validate e a
failure = Failure

success :: a -> Validate e a
success = Success

validate :: (e -> z) -> (a -> z) -> Validate e a -> z
validate err _ (Failure e) = err e
validate _ suc (Success a) = suc a
