module Handler.Hub.Util where

import Import
import Control.Applicative (Applicative(..), (<|>), some)
import Data.Char (isDigit)
import Data.List.NonEmpty
import Data.String (fromString)
import qualified Data.Semigroup as S
import Text.Parsec.Text ()
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

url :: Text -> Maybe Text -> Validation Text Text
url key opt = validateUrl key -<< param key opt

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
