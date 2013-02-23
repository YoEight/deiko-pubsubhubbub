module PubSubHubBub.XML.Atom where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad.Trans

import           Data.Foldable       hiding (foldl)
import           Data.Machine        hiding (Source (..))
import qualified Data.Map            as M

import           Text.XML.HXT.Core

data Id = Id String deriving Show

data Title = Title String deriving Show

data Updated = Updated String deriving Show

data Link = Link String String
          | SimpleLink String deriving Show

data Source = Source { sourceAttrs :: M.Map String AtomLit } deriving Show

data AtomLit = AId Id
             | ATitle Title
             | AUpdated Updated
             | AContent String
             | ALink Link
             | ASource Source
             | AAuthor String
             | ASummary String deriving Show

data FeedLit = FAtom AtomLit
             | FEntry Entry deriving Show

data Feed = Feed { feedAttrs   :: M.Map String AtomLit
                 , feedEntries :: [Entry] } deriving Show

data Entry = Entry { entryAttrs :: M.Map String AtomLit } deriving Show

instance XmlPickler Id where
  xpickle = xpElem "id" $
            xpWrap (Id, \(Id x) -> x) xpText0

instance XmlPickler Title where
  xpickle = xpElem "title" $
            xpWrap (Title, \(Title x) -> x) xpText0

instance XmlPickler Updated where
  xpickle = xpElem "updated" $
            xpWrap (Updated, \(Updated x) -> x) xpText0

instance XmlPickler Source where
  xpickle = xpElem "source" $
            xpWrap (Source . M.fromList . fmap atomLitMapping, fmap snd . M.toList . sourceAttrs) $
            xpickle

instance XmlPickler Link where
  xpickle = xpElem "link" $
            xpWrap (toLink, fromLink) $
            xpPair (xpOption $ xpAttr "rel" xpText0) (xpAttr "href" xpText0)
    where
      toLink (Just r, h) = Link r h
      toLink (_, h)      = SimpleLink h
      fromLink (Link r h)     = (Just r, h)
      fromLink (SimpleLink h) = (Nothing, h)

instance XmlPickler AtomLit where
  xpickle =
    let a_id      = xpWrap (AId, \(AId x) -> x) xpickle
        a_title   = xpWrap (ATitle, \(ATitle x) -> x) xpickle
        a_updated = xpWrap (AUpdated, \(AUpdated x) -> x) xpickle
        a_content = xpElem "content" $
                    xpWrap (AContent, \(AContent x) -> x) xpText0
        a_link    = xpWrap (ALink, \(ALink x) -> x) xpickle
        a_source  = xpWrap (ASource, \(ASource x) -> x) xpickle
        a_author  = xpElem "author" $
                    xpElem "name" $
                    xpWrap (AAuthor, \(AAuthor x) -> x) xpText0
        a_summary = xpElem "summary" $
                    xpWrap (ASummary, \(ASummary x) -> x) xpText0
        selector (AId _)      = 0
        selector (ATitle _)   = 1
        selector (AUpdated _) = 2
        selector (AContent _) = 3
        selector (ALink _)    = 4
        selector (ASource _)  = 5
        selector (AAuthor _)  = 6
        selector (ASummary _) = 7
    in xpAlt selector [a_id, a_title, a_updated, a_content, a_link, a_source, a_author, a_summary]

instance XmlPickler Entry where
  xpickle =
    xpWrap (Entry . M.fromList . fmap atomLitMapping, fmap snd . M.toList . entryAttrs) $
    xpElem "entry" $
    xpickle

instance XmlPickler FeedLit where
  xpickle =
    let atom    = xpWrap (FAtom, \(FAtom x) -> x) xpickle
        entries = xpWrap (FEntry, \(FEntry x) -> x) xpickle
        selector (FAtom _)  = 0
        selector (FEntry _) = 1
    in xpAlt selector [atom, entries]

instance XmlPickler Feed where
  xpickle =
    xpElemNS "" "atom" "feed" $
    xpWrap (listToFeed, feedToList) $
    xpickle
    where
      feedToList (Feed map entries) =
        (fmap (FAtom . snd) $ M.toList map) ++ (fmap FEntry entries)
      listToFeed xs =
        let (map, entries) = foldl go (M.empty, []) xs
            go (map, es) x =
              case x of
                FEntry e -> (map, e:es)
                r        ->
                  let (key, value) = mapping r
                  in (M.insert key value map, es)
            mapping (FAtom a) = atomLitMapping a
        in Feed map entries

isLink :: String -> Bool
isLink "hub"  = True
isLink "self" = True
isLink _      = False

atomLitMapping :: AtomLit -> (String, AtomLit)
atomLitMapping x@(AId _)      = ("id", x)
atomLitMapping x@(ATitle _)   = ("title", x)
atomLitMapping x@(AUpdated _) = ("updated", x)
atomLitMapping x@(AContent _) = ("content", x)
atomLitMapping x@(ALink _)    = ("link", x)
atomLitMapping x@(ASummary _) = ("summary", x)
atomLitMapping x@(ASource _)  = ("source", x)
atomLitMapping x@(AAuthor _)  = ("author", x)

parseAtomFeed :: MonadIO m => String -> Machine m Feed
parseAtomFeed src = construct go
  where
    go = do
      feeds <- liftIO $ runX (application src)
      traverse_ yield feeds
      empty

application :: String -> IOSArrow b Feed
application src =
  configSysVars [withTrace 2, withRemoveWS yes]
  >>> readString [withValidate no, withSubstDTDEntities  no] src
  >>> getChildren
  >>> isElem
  >>> xunpickleVal xpickle
