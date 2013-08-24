--------------------------------------------------------------------
-- |
-- Module      : Flickr.Utils
-- Description : assorted functions for unravelling Flickr responses.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer  : Sigbjorn Finne <sof@forkIO.com>
-- Stability   : provisional
-- Portability : portable
--
-- Assorted functions for unravelling Flickr responses.
--------------------------------------------------------------------
module Flickr.Utils
       ( module Flickr.Utils
       , fromMaybe
       ) where

import Text.XML.Light as XML
import Data.Maybe
import Data.List
import Control.Monad

--import Flickr.Monad ( ErrM, FlickErr(..), FlickErrorType(..), flickError )

pNodes       :: String -> [XML.Element] -> [XML.Element]
pNodes x es   = filter ((nsName x ==) . elName) es

pNode        :: String -> [XML.Element] -> Maybe XML.Element
pNode x es    = listToMaybe (pNodes x es)

pLeaf        :: String -> [XML.Element] -> Maybe String
pLeaf x es    = strContent `fmap` pNode x es

pAttr        :: String -> XML.Element -> Maybe String
pAttr x e     = lookup (nsName x) [ (k,v) | Attr k v <- elAttribs e ]

pMany        :: String -> (XML.Element -> Maybe a) -> [XML.Element] -> [a]
pMany p f es  = mapMaybe f (pNodes p es)

children     :: XML.Element -> [XML.Element]
children e    = onlyElems (elContent e)

child :: XML.Element -> XML.Element
child e =
  case children e of
    [] -> error ("child: empty content " ++ show e)
    (x:_) -> x

nsName :: String -> QName
nsName x = QName{qName=x,qURI=Nothing,qPrefix=Nothing}

without :: [String] -> [XML.Attr] -> [XML.Attr]
without xs as = filter (\ a -> not (attrKey a `elem` qxs)) as
 where
  qxs = map nsName xs

ifNamed :: String -> XML.Element -> Maybe a -> Maybe a
ifNamed s e n = guard (elName e == nsName s) >> n

mbDef :: a -> Maybe a -> Maybe a
mbDef x Nothing = Just x
mbDef _ v = v

readMb :: Read a => String -> Maybe a
readMb x = case reads x of
             ((v,_):_) -> Just v
	     _ -> Nothing

piped :: [String] -> String
piped xs = concat (intersperse "|" xs)

opt :: String -> String -> Maybe (String,String)
opt a b = Just (a,b)

optB :: String -> Bool -> Maybe (String,String)
optB _ False = Nothing
optB a _ = Just (a,"")

opt1 :: String -> [String] -> Maybe (String,String)
opt1 _ [] = Nothing
opt1 a b = Just (a,piped b)

mbOpt :: String -> (a -> String) -> Maybe a -> Maybe (String,String)
mbOpt _tg _ Nothing = Nothing
mbOpt tg f (Just x) = Just (tg,f x)

showBool :: Bool -> String
showBool x = show (fromEnum x)

intContent :: Element -> Maybe Int
intContent e =
  case reads (strContent e) of
    ((v,_):_) -> Just v
    _ -> Nothing
