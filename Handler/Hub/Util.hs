module Handler.Hub.Util where

import Import
import Control.Applicative (Applicative(..))
import Data.Char (isDigit)
import Data.List.NonEmpty
import qualified Data.Semigroup as S

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

param :: Text -> Maybe a -> Validation Text a
param _ (Just a) = Success a
param key _      = Failure (nel $ key <> " is not provided")

nonEmpty :: Text -> [Text] -> Validation Text (NonEmpty Text)
nonEmpty _ (x:xs) = Success (x :| xs)
nonEmpty key _    = Failure (nel $ key <> " is not provided")

number :: Text -> Text -> Validation Text Int
number key value = 
  case show value of
    []                  -> notNumber
    xs | all isDigit xs -> Success (read xs)
       | otherwise      -> notNumber
  where
    notNumber = Failure (nel $ key <> " is not a member [" <> value <> "]")

(>>-) :: Validation e a -> (a -> Validation e b) -> Validation e b
Failure e >>- _ = Failure e
Success a >>- f = f a

(-<<) :: (a -> Validation e b) -> Validation e a -> Validation e b
f -<< m = m >>- f

validation :: (NonEmpty e -> x) -> (a -> x) -> Validation e a -> x
validation err _ (Failure e) = err e
validation _ k   (Success a) = k a

nel :: a -> NonEmpty a
nel a = a :| []