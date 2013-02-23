module PubSubHubBub.XML.Atom where

import           Control.Arrow

import qualified Data.Map          as M

import           Text.XML.HXT.Core

data AtomLit = AId String
             | ATitle String
             | AUpdated String
             | AContent String
             | ALink (Maybe String) String
             | ASummary String deriving Show

data FeedLit = FPair String String
             | FUpdated String
             | FEntry Entry deriving Show

data Feed = Feed { feedAttrs   :: M.Map String String
                 , feedEntries :: [Entry] } deriving Show

data Entry = Entry { entryAttrs :: M.Map String AtomLit } deriving Show

instance XmlPickler AtomLit where
  xpickle =
    let a_id      = xpElem "id" $
                    xpWrap (AId, \(AId x) -> x) xpText0
        a_title   = xpElem "title" $
                    xpWrap (ATitle, \(ATitle x) -> x) xpText0
        a_updated = xpElem "updated" $
                    xpWrap (AUpdated, \(AUpdated x) -> x) xpText0
        a_content = xpElem "content" $
                    xpWrap (AContent, \(AContent x) -> x) xpText0
        a_link    = xpElem "link" $
                    xpWrap (uncurry ALink, \(ALink k v) -> (k, v)) $
                    xpPair (xpOption $ xpAttr "rel" xpText0) (xpAttr "href" xpText0)
        a_summary = xpElem "summary" $
                    xpWrap (ASummary, \(ASummary x) -> x) xpText0
        selector (AId _)      = 0
        selector (ATitle _)   = 1
        selector (AUpdated _) = 2
        selector (AContent _) = 3
        selector (ALink _ _)  = 4
        selector (ASummary _) = 5
    in xpAlt selector [a_id, a_title, a_updated, a_content, a_link, a_summary]

instance XmlPickler Entry where
  xpickle =
    xpWrap (Entry . M.fromList . fmap tupled, fmap snd . M.toList . entryAttrs) $
    xpElem "entry" $
    xpickle
    where
      tupled x@(AId _)      = ("id", x)
      tupled x@(ATitle _)   = ("title", x)
      tupled x@(AUpdated _) = ("updated", x)
      tupled x@(AContent _) = ("content", x)
      tupled x@(ALink _ _)  = ("link", x)
      tupled x@(ASummary _) = ("summary", x)

instance XmlPickler FeedLit where
  xpickle =
    let pair    = xpElem "link" $
                  xpWrap (uncurry FPair, \(FPair k v) -> (k, v)) $
                  xpPair (xpAttr "rel" xpText0) (xpAttr "href" xpText0)
        updated = xpElem "updated" $
                  xpWrap (FUpdated, \(FUpdated x) -> x) xpText0
        entries = xpWrap (FEntry, \(FEntry x) -> x) xpickle
        selector (FPair _ _)  = 0
        selector (FUpdated _) = 1
        selector (FEntry _) = 2
    in xpAlt selector [pair, updated, entries]

instance XmlPickler Feed where
  xpickle =
    xpElemNS "" "atom" "feed" $
    xpWrap (listToFeed, feedToList) $
    xpickle
    where
      feedToList (Feed map entries) =
        (fmap toLit $ M.toList map) ++ (fmap FEntry entries)
      toLit (k, v)
        | isLink k = FPair k v
        | k == "updated" = FUpdated v
        | otherwise      = error $ "Not supported FeedLiteral -> " ++ k
      listToFeed xs =
        let go map es []     = Feed map es
            go map es (x:xs) =
              case x of
                FEntry e -> go map (e:es) xs
                r        ->
                  let (key, value) = mapping r
                  in go (M.insert key value map) es xs
            mapping (FPair k v)  = (k, v)
            mapping (FUpdated v) = ("updated", v)
        in go M.empty [] xs

isLink :: String -> Bool
isLink "hub"  = True
isLink "self" = True
isLink _      = False

location = "/Users/yoeight/Desktop/atom.xml"

main = runX (application location)

application :: String -> IOSArrow b Feed
application src =
  configSysVars [withTrace 2, withRemoveWS yes]
  >>> readDocument [withValidate no, withSubstDTDEntities  no] src
  >>> getChildren
  >>> isElem
  >>> xunpickleVal xpickle
