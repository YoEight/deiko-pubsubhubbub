module Web.Deiko.Hub.XML.Atom where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad.Trans

import           Data.Foldable       hiding (foldl)
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

atomLitMapping :: AtomLit -> (String, AtomLit)
atomLitMapping x@(AId _)      = ("id", x)
atomLitMapping x@(ATitle _)   = ("title", x)
atomLitMapping x@(AUpdated _) = ("updated", x)
atomLitMapping x@(AContent _) = ("content", x)
atomLitMapping x@(ALink _)    = ("link", x)
atomLitMapping x@(ASummary _) = ("summary", x)
atomLitMapping x@(ASource _)  = ("source", x)
atomLitMapping x@(AAuthor _)  = ("author", x)

feedHub :: Feed -> Maybe String
feedHub = fmap go . (M.lookup "hub") . feedAttrs
  where
    go (ALink (Link _ v)) = v

feedSelf :: Feed -> Maybe String
feedSelf = fmap go . M.lookup "self" . feedAttrs
  where
    go (ALink (Link _ v)) = v

feedUpdated :: Feed -> String
feedUpdated = go . (M.! "updated") . feedAttrs
  where
    go (AUpdated (Updated u)) = u

feedSource :: Feed -> Maybe Source
feedSource = fmap go . M.lookup "source" . feedAttrs
  where
    go (ASource s) = s

feedTitle :: Feed -> Maybe String
feedTitle = fmap go . M.lookup "title" . feedAttrs
  where
    go (ATitle (Title t)) = t

feedId :: Feed -> Maybe String
feedId = fmap go . M.lookup "id" . feedAttrs
  where
    go (AId (Id i)) = i

entrySource :: Entry -> Maybe Source
entrySource = fmap go . M.lookup "source" . entryAttrs
  where
    go (ASource s) = s

entryTitle :: Entry -> String
entryTitle = go . (M.! "title") . entryAttrs
  where
    go (ATitle (Title t)) = t

entryLink :: Entry -> String
entryLink = go . (M.! "link") . entryAttrs
  where
    go (ALink (SimpleLink l)) = l

entryId :: Entry -> String
entryId = go . (M.! "id") . entryAttrs
  where
    go (AId (Id i)) = i

entryUpdated :: Entry -> String
entryUpdated = go . (M.! "updated") . entryAttrs
  where
    go (AUpdated (Updated u)) = u

entryContent :: Entry -> Maybe String
entryContent = fmap go . (M.lookup "content") . entryAttrs
  where
    go (AContent c) = c

entrySummary :: Entry -> Maybe String
entrySummary = fmap go . (M.lookup "summary") . entryAttrs
  where
    go (ASummary s) = s

application :: String -> IOSArrow b Feed
application src =
  configSysVars [withTrace 2, withRemoveWS yes]
  >>> readString [withValidate no, withSubstDTDEntities  no] src
  >>> getChildren
  >>> isElem
  >>> xunpickleVal xpickle
