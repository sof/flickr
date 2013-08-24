--------------------------------------------------------------------
-- |
-- Module      : Flickr.Groups.Pools
-- Description : flickr.groups.pools - manage photo group pooling.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer  : Sigbjorn Finne <sof@forkIO.com>
-- Stability   : provisional
-- Portability : portable
--
-- flickr.groups.pools API, manage photo group pooling.
--------------------------------------------------------------------
module Flickr.Groups.Pools where

import Flickr.Monad
import Flickr.Types
import Flickr.Types.Import

import Control.Monad ( liftM )

-- | Add a photo to a group's pool.
add :: PhotoID -> GroupID -> FM ()
add pid gid = withWritePerm $ postMethod $ do
   flickCall_ "flickr.groups.pools.add"
              [ ("photo_id", pid)
	      , ("group_id", gid)
	      ]

-- | Returns next and previous photos for a photo in a group pool.
getContext :: PhotoID -> GroupID -> FM (Photo,Photo)
getContext pid gid = 
 flickTranslate toPhotoPair $
  flickrCall "flickr.groups.pools.getContext"
              [ ("photo_id", pid)
	      , ("group_id", gid)
	      ]
             
-- | Returns a list of groups to which you can add photos.
getGroups :: FM [Group]
getGroups = 
 flickTranslate toGroupList $
  flickrCall "flickr.groups.pools.getGroups"
             []

-- | Returns a list of pool photos for a given group, 
-- based on the permissions of the group and the user logged in (if any).
getPhotos :: GroupID -> [Tag] -> Maybe UserID -> [PhotoInfo] -> FM [Photo]
getPhotos gid ts uid ps = liftM snd $
  flickTranslate toPhotoList $
   flickrCall "flickr.groups.Pools.getPhotos"
              (lsArg "tags" ts $
	        mbArg "user_id" uid $
		 lsArg "extras" (map show ps)
  	               [ ("group_id", gid) ])

-- | Remove a photo from a group pool.
remove :: PhotoID -> GroupID -> FM ()
remove pid gid = withWritePerm $ postMethod $ 
   flickCall_ "flickr.groups.pools.remove"
              [ ("photo_id", pid)
	      , ("group_id", gid)
	      ]
