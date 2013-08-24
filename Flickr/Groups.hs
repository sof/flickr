--------------------------------------------------------------------
-- |
-- Module    : Flickr.Groups
-- Description : flickr.groups - search and browse groups/categories.
-- Copyright : (c) Sigbjorn Finne, 2008
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- flickr.groups API, traversing photo categories and groups.
--------------------------------------------------------------------
module Flickr.Groups where

import Flickr.Monad
import Flickr.Types
import Flickr.Types.Import

browse :: Maybe CategoryID -> FM Category
browse cid = withReadPerm $
 flickTranslate toCategory $
  flickrCall "flickr.groups.browse"
             (mbArg "cat_id" cid [])

getInfo :: GroupID -> Maybe String -> FM Group
getInfo gid lang = 
  flickTranslate toGroup $
   flickrCall "flickr.groups.getInfo"
              (mbArg "lang" lang [("group_id", gid)])

search :: String
       -> FM [Group]
search txt = 
  flickTranslate toGroupList $
   flickrCall "flickr.groups.search"
              [("text", txt)]
