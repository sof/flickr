--------------------------------------------------------------------
-- |
-- Module      : Flickr.Favorites
-- Description : flickr.favorites - manage favorite photos.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer  : Sigbjorn Finne <sof@forkIO.com>
-- Stability   : provisional
-- Portability : portable
--
-- flickr.favorites API, managing a user's favorite photos.
--------------------------------------------------------------------
module Flickr.Favorites 
       ( add            -- :: PhotoID -> FM ()
       , remove         -- :: PhotoID -> FM ()
       
       , getList        -- :: Maybe UserID -> [PhotoInfo] -> DateDetails -> FM (PhotoContext, [Photo])
       , getPublicList  -- :: UserID -> [PhotoInfo] -> DateDetails -> FM (PhotoContext, [Photo])
       ) where

import Flickr.Monad
import Flickr.Types
import Flickr.Types.Import

-- | Adds a photo to a user's favorites list.
add :: PhotoID -> FM ()
add pid = withWritePerm $ postMethod $
  flickCall_ "flickr.favorites.add"
             [ ("photo_id", pid) ]

-- | Removes a photo from a user's favorites list.
remove :: PhotoID -> FM ()
remove pid = withWritePerm $ postMethod $
  flickCall_ "flickr.favorites.remove"
             [ ("photo_id", pid) ]

-- | Returns a list of the user's favorite photos. 
-- Only photos which the calling user has permission to see are returned.
getList :: Maybe UserID
        -> [PhotoInfo]
	-> DateDetails
	-> FM (PhotoContext, [Photo])
getList uid ps mbDates = withReadPerm $
  flickTranslate toPhotoList $
   flickrCall "flickr.favorites.getList"
              (mbArg "user_id" uid $
               mbArg "min_fave_date" (dateMinTaken mbDates) $
	       mbArg "max_fave_date" (dateMaxTaken mbDates) $
	        lsArg "extras" (map show ps) [])
	
-- | Returns a list of favorite public photos for the given user.
getPublicList :: UserID
              -> [PhotoInfo]
	      -> DateDetails
              -> FM (PhotoContext, [Photo])
getPublicList uid ps mbDates = 
  flickTranslate toPhotoList $
    flickrCall "flickr.favorites.getPublicList"
             (mbArg "min_fave_date" (dateMinTaken mbDates) $
	      mbArg "max_fave_date" (dateMaxTaken mbDates) $
	      lsArg "extras" (map show ps) $
	        [("user_id", uid)])

	
