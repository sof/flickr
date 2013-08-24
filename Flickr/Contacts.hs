--------------------------------------------------------------------
-- |
-- Module      : Flickr.Contacts
-- Description : flickr.contacts - fetch user's contact list.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer  : Sigbjorn Finne <sof@forkIO.com>
-- Stability   : provisional
-- Portability : portable
--
-- flickr.contacts API, fetching a user's contact list.
--------------------------------------------------------------------
module Flickr.Contacts where

import Flickr.Monad
import Flickr.Types
import Flickr.Types.Import

import Data.Maybe (maybeToList)

-- | Get a list of contacts for the calling user.
getList :: Maybe Filter -> FM [Contact]
getList f = withReadPerm $
  flickTranslate toContactList $
    flickCall "flickr.contacts.getList"
              (maybeToList (fmap (\ x -> ("filter",show x)) f))
	
-- | Get the contact list for a user.
getPublicList :: UserID -> FM [Contact]
getPublicList u = 
  flickTranslate toContactList $
    flickCall "flickr.contacts.getPublicList"
              [ ("user_id", u) ]
