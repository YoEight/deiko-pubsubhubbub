module Web.Hub.Feed where

import Control.Applicative
import Control.Monad.State

import Data.ByteString     (readFile)
import Data.Foldable
import Data.Maybe          (maybeToList)
import Text.XML.Light

import Prelude             hiding (readFile)

data Feed = Feed { feedId      :: Maybe String
                 , feedEntries :: [Entry] } deriving Show

data Author = Author { authorEmail :: String  } deriving Show

data Entry = Entry { entryId      :: Maybe String
                   , entryAuthor  :: Maybe Author
                   , entryTitle   :: Maybe String
                   , entryUpdated :: Maybe String } deriving Show

data AuthorState = AuthorState { asEmail :: Maybe String } deriving Show

atom :: String -> QName
atom name = (QName name (Just "http://www.w3.org/2005/Atom") (Just "atom"))

parseFeed :: Element -> Maybe Feed
parseFeed e@(Element (QName "feed" _ (Just "atom")) _ nodes _) =
  let fId     = fmap strContent (findChild (atom "id") e)
      entries = parseEntries (onlyElems nodes)
  in Just (Feed fId entries)
parseFeed _ = Nothing

parseEntries :: [Element] -> [Entry]
parseEntries = foldMap go
  where
    go (Element (QName "entry" _ (Just "atom")) _ nodes _) =
      let elms = onlyElems nodes
          toto = error $ show elms
      in  [parseEntry elms]
    go x = []

initEntryState = Entry Nothing Nothing Nothing Nothing
initAuthorState = AuthorState Nothing

directStrContent :: [Content] -> Maybe String
directStrContent ((Text (CData _ str _)):_) = return str
directStrContent _ = Nothing

parseEntry :: [Element] -> Entry
parseEntry xs = execState (traverse_ go xs) initEntryState
  where
    go (Element (QName "author" _ (Just "atom")) _ nodes _) =
      traverse_ (\a -> modify $ \s -> s { entryAuthor = Just a })
                  (parseAuthor (onlyElems nodes))

    go (Element (QName "id" _ (Just "atom")) _ nodes _) =
      traverse_ (\str -> modify $ \s -> s { entryId = Just str })
                  (directStrContent nodes)

    go (Element (QName "title" _ (Just "atom")) _ nodes _) =
      traverse_ (\str -> modify $ \s -> s { entryTitle = Just str })
                  (directStrContent nodes)

    go (Element (QName "updated" _ (Just "atom")) _ nodes _) =
      traverse_ (\str -> modify $ \s -> s { entryUpdated = Just str })
                  (directStrContent nodes)

    go _ = return ()

parseAuthor :: [Element] -> Maybe Author
parseAuthor xs =
  fmap Author $ asEmail $ execState (traverse_ go xs) initAuthorState
    where
      go (Element (QName "email" _ (Just "atom")) _ nodes _) =
        traverse_ (\str -> modify $ \s -> s { asEmail = Just str })
                    (directStrContent nodes)
