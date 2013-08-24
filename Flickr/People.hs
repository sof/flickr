--------------------------------------------------------------------
-- |
-- Module      : Flickr.People
-- Description : flickr.people - access a user's attributes etc.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer  : Sigbjorn Finne <sof@forkIO.com>
-- Stability   : provisional
-- Portability : portable
--
-- flickr.people API, accessing a user's attributes etc.
-- <http://www.flickr.com/services/api/>
--------------------------------------------------------------------
module Flickr.People where

import Flickr.Monad
import Flickr.Types
import Flickr.Types.Import

import Flickr.Utils
import Text.XML.Light.Proc   ( findChild )
import Control.Monad

-- | Return a user's NSID, given their email address
findByEmail :: {-EMail-}String -> FM User
findByEmail e = 
 flickTranslate toUser $
   flickrCall "flickr.people.findByEmail"
              [ ("find_email", e) ]

-- | Return a user's NSID, given their username.
findByUsername :: UserName -> FM User
findByUsername u = 
 flickTranslate toUser $
   flickrCall "flickr.people.findByUsername"
              [ ("username", u) ]

-- | Get information about a user.
getInfo :: UserID -> Bool -> FM User
getInfo uid authCall = (if authCall then withReadPerm else id) $ do
  flickTranslate toUser $
    flickCall "flickr.people.getInfo" 
              [ ("user_id", uid) ]

-- | Returns the list of public groups a user is a member of.
getPublicGroups :: UserID -> FM [Group]
getPublicGroups uid = 
  flickTranslate toGroupList $
    flickrCall "flickr.people.getPublicGroups"
               [ ("user_id", uid) ]

-- | Get a list of public photos for the given user.
getPublicPhotos :: UserID -> Maybe Safety -> [PhotoInfo] -> FM [Photo]
getPublicPhotos uid s ps = liftM snd $
  flickTranslate toPhotoList $
    flickrCall "flickr.people.getPublicPhotos"
               (mbArg "safe_search" (fmap (show.succ.fromEnum) s) $
	         lsArg "extras" (map show ps) 
		   [ ("user_id", uid) ])

-- | Returns information for the calling user related to photo uploads.
getUploadStatus :: FM (User, Bandwidth, FileSize, PhotosetQuota)
getUploadStatus = 
  flickTranslate toRes $
    flickrCall "flickr.people.getUploadStatus" []
 where
  toRes s = parseDoc eltRes s
  
  eltRes e = do
    u <- eltUser e
    b <- findChild (nsName "bandwidth") e >>= eltBandwidth 
    f <- findChild (nsName "filesize") e >>= eltFileSize
    s <- findChild (nsName "sets") e >>= eltPhotosetQuota
    return (u,b,f,s)
    
    
    
