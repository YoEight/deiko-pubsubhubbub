module PubSubHubBub.Verification where

import           Control.Monad.State

import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as C
import           Data.Char
import           Data.Foldable              hiding (foldr1)
import           Data.Machine
import qualified Data.Map                   as M
import           Data.Maybe
import           Data.Word

import           Control.Monad.Random
import           Control.Monad.Random.Class
import           Control.Monad.Trans
import           Control.Monad.Trans.Either

import           PubSubHubBub.Types

import           System.Random

import           Network.Curl

verification :: SubReq -> Machine (EitherT String IO) SubReq
verification req =
  let http   = fmap validateVerifyResponse $ verifyHttpRequest req
      either = fmap (const req) $ EitherT http
  in lift either

insertOptional :: (a -> Maybe ParamLit)
               -> B.ByteString
               -> a
               -> M.Map B.ByteString B.ByteString
               -> M.Map B.ByteString B.ByteString
insertOptional k key t map = maybe map go (k t)
  where
    go (PBytes xs) = M.insert key xs map
    go (PInt i)    = M.insert key (packString (show i)) map

insertLit :: (a -> ParamLit)
          -> B.ByteString
          -> a
          -> M.Map B.ByteString B.ByteString
          -> M.Map B.ByteString B.ByteString
insertLit k key t = M.insert key (go $ k t)
  where
    go (PBytes xs) = xs
    go (PInt i)    = packString (show i)

verifyHttpRequest :: SubReq -> IO (Int, B.ByteString, B.ByteString)
verifyHttpRequest req = do
  params <- confirmationRequestParams req
  let callback     = subReqCallbackUrl req
      query_string = makeQueryString params
      url          = callback ++ query_string
  response <- withCurlDo $ curlGetResponse_ url [] :: IO (CurlResponse_ [(String, String)] B.ByteString)
  return (respStatus response, params M.! hub_challenge, respBody response)

validateVerifyResponse :: (Int, B.ByteString, B.ByteString) -> Either String ()
validateVerifyResponse (ret, challenge, content)
  | ret == 404              = Left "verification: unreachable callback url"
  | 200 >= ret && ret < 300 = validateVerifyContent challenge content
  | otherwise               = Left "verification: error during confirmation"

validateVerifyContent :: B.ByteString -> B.ByteString -> Either String ()
validateVerifyContent challenge = go . B.breakSubstring (B.snoc hub_challenge equal)
  where
    go (ans, challenge')
      | not (B.null ans) && challenge == challenge' = Right ()
      | otherwise                                   = Left "verification: challenge paraphrase doesn't match"

confirmationRequestParams :: SubReq -> IO (M.Map B.ByteString B.ByteString)
confirmationRequestParams req = execStateT go M.empty
  where
    go = do
      challenge <- lift $ randomString
      modify (insertLit subReqMode hub_mode req)
      modify (insertLit subReqTopic hub_topic req)
      modify (insertOptional subReqLeaseSeconds hub_lease_seconds req)
      modify (insertOptional subReqVerifyToken hub_verify_token req)
      modify (M.insert hub_challenge (packString challenge))

subReqCallbackUrl :: SubReq -> String
subReqCallbackUrl = go . subReqCallback
  where
    go (PBytes xs) = C.unpack xs

makeQueryString :: M.Map B.ByteString B.ByteString -> String
makeQueryString = ('?':) . foldr1 amp . fmap query . M.toList
  where
    query (key, value) = (C.unpack key) ++ "=" ++ (C.unpack value)
    amp q q' = q ++ "&" ++ q'

randomString :: IO String
randomString = do
  values <- evalRandIO (sequence $ replicate 10 rnd)
  return $ map chr values
    where
      rnd :: RandomGen g => Rand g Int
      rnd = getRandomR (65, 90) -- A-Za-z

-- This is very unfortunate. But not all dependencies have migrated to bytestring 0.10.2.*
packString :: String -> B.ByteString
packString = B.pack . fmap (fromInteger . toInteger . ord)

equal :: Word8
equal = 61
