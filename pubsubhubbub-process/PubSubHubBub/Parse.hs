{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

module PubSubHubBub.Parse where

import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans

import           Data.Bifunctor
import qualified Data.ByteString      as B
import           Data.Either
import           Data.Foldable
import           Data.Machine
import qualified Data.Map             as M

import           PubSubHubBub.Types

parseLiteral :: Process (B.ByteString, B.ByteString) (B.ByteString, ParamLit)
parseLiteral = repeatedly (await >>= go)
  where
    go (key, value) =
      either (const $ yield (key, PBytes value)) (yield . (key,)) (PInt <$> parseInt value)

parseParams :: Process (B.ByteString, B.ByteString) [(B.ByteString, ParamLit)]
parseParams = (repeatedly $ go []) <~ parseLiteral
  where
    go xs = do
      x <- await <|> yield (reverse xs) *> empty
      go (x:xs)

makeMapParam :: (B.ByteString -> ParamLit -> ParamLit -> ParamLit)
             -> Process [(B.ByteString, ParamLit)] (M.Map B.ByteString ParamLit)
makeMapParam k = repeatedly (await >>= go)
  where
    go [] = empty
    go xs =
      let map = execState (traverse_ makeMap xs) M.empty
      in yield map *> empty
    makeMap (key, lit) = modify (step key lit)
    step key = M.insertWith (k key) key

makeSubReq :: Process [(B.ByteString, ParamLit)] (Either String SubReq)
makeSubReq = make validateSubReqParams <~ makeMapParam subReqSelector

makeVerif :: Process [(B.ByteString, ParamLit)] (Either String Verif)
makeVerif = make validateVerifParams <~ makeMapParam youngerSelector

makeNotif :: Process [(B.ByteString, ParamLit)] (Either String Notif)
makeNotif = make validateNotifParams <~ makeMapParam youngerSelector

make :: ReaderT (M.Map B.ByteString ParamLit) (Either String) a
     -> Process (M.Map B.ByteString ParamLit) (Either String a)
make valid = repeatedly $ await >>= go
  where
    go map =
      let validation = runReaderT valid map
      in yield validation *> empty

subReqSelector :: B.ByteString
               -> ParamLit
               -> ParamLit
               -> ParamLit
subReqSelector key new old
  | key == hub_verify = merge new old
  | otherwise         = new

youngerSelector :: B.ByteString
                -> ParamLit
                -> ParamLit
                -> ParamLit
youngerSelector = const const

merge :: ParamLit -> ParamLit -> ParamLit
merge n (PList xs) = PList (xs ++ [n])
merge n o          = PList [o, n]
