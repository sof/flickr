--------------------------------------------------------------------
-- |
-- Module      : Flickr.URLs
-- Description : flickr.urls - locate user photos and groups.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer  : Sigbjorn Finne <sof@forkIO.com>
-- Stability   : provisional
-- Portability : portable
-- 
-- The 'flickr.urls' API + convenience functions for creating
-- URL strings to various Flickr std-format URLs.
--------------------------------------------------------------------
module Flickr.URLs 
       ( getGroup       -- :: GroupID -> FM URLString
       , getUserPhotos  -- :: Maybe UserID -> FM URLString
       , getUserProfile -- :: Maybe UserID -> FM URLString
       , lookupGroup    -- :: URLString -> FM Group
       , lookupUser     -- :: URLString -> FM User
       
       , photoSourceURL     -- :: PhotoDetails -> PhotoSize -> URLString
       , userProfilePageURL -- :: User -> URLString
       , userPhotoStreamURL -- :: User -> URLString
       , userPhotoURL       -- :: User -> PhotoID -> URLString
       , userPhotosetsURL   -- :: User -> URLString
       , userPhotosetURL    -- :: User -> PhotosetID -> URLString
       ) where

import Flickr.Monad
import Flickr.Types
import Flickr.Types.Import

import Data.Maybe

-- | Returns the url to a group's page.
getGroup :: GroupID -> FM URLString
getGroup gid = do
  flickTranslate (toString "url") $
     flickCall "flickr.urls.getGroup" [("group_id", gid)]

-- | Returns the url to a user's photos.
getUserPhotos :: Maybe UserID -> FM URLString
getUserPhotos mb = do
  flickTranslate (toString "url") $
    flickCall "flickr.urls.getUserPhotos" 
              (maybeToList (fmap (\ x -> ("user_id", x)) mb))

-- | Returns the url to a user's profile.
getUserProfile :: Maybe UserID -> FM URLString
getUserProfile mb = do
  flickTranslate (toString "url") $
    flickCall "flickr.urls.getUserProfile"
              (maybeToList (fmap (\ x -> ("user_id", x)) mb))

-- | Returns a group NSID, given the url to a group's page or photo pool.
lookupGroup :: URLString -> FM Group
lookupGroup u = do
  flickTranslate toGroup $ 
    flickCall "flickr.urls.lookupGroup" [("url", u)]

-- | Returns a user NSID, given the url to a user's photos or profile.
lookupUser :: URLString -> FM User
lookupUser u = do
  flickTranslate toUser $
     flickCall "flickr.urls.lookupUser" [("url", u)]

photoSourceURL :: PhotoDetails -> PhotoSize -> URLString
photoSourceURL p sz = 
 case photoURL ph of
   Just u -> u
   _ -> 
     -- resisting the temptation to pull in the uri-template 
     -- package here...
    "http://farm" ++ fid ++ ".static.flickr.com/" ++ sid ++ 
    '/':pid ++ '_':sec ++ suff
 where
  ph  = photoDetailsPhoto p

  fid = fromMaybe "1" (photoFarm ph)
  sid = fromMaybe "1" (fmap show $ photoServer ph)
  pid = photoId ph
  sec = 
   case sz of
     PhotoSizeOriginal -> fromMaybe "1" (photoDetailsOrigSecret p)
     _ -> photoSecret ph
  suff = 
   case sz of
     PhotoSizeMedium -> ".jpg"
     PhotoSizeOriginal -> "_o." ++ fromMaybe "jpg" (photoDetailsOrigFormat p)
     PhotoSizeSmallSquare -> "_s.jpg"
     PhotoSizeThumb -> "_t.jpg"
     PhotoSizeSmall -> "_m.jpg"
     PhotoSizeLarge -> "_b.jpg"

userProfilePageURL :: User -> URLString
userProfilePageURL u = 
  "http://www.flickr.com/people/" ++ userId u ++ "/"

userPhotoStreamURL :: User -> URLString
userPhotoStreamURL u =
  "http://www.flickr.com/photos/" ++ userId u ++ "/"

userPhotoURL :: User -> PhotoID -> URLString
userPhotoURL u pid = 
  "http://www.flickr.com/photos/" ++ userId u ++ '/':pid

userPhotosetsURL :: User -> URLString
userPhotosetsURL u = 
  "http://www.flickr.com/photos/" ++ userId u ++ "/sets/"

userPhotosetURL :: User -> PhotosetID -> URLString
userPhotosetURL u p = 
  "http://www.flickr.com/photos/" ++ userId u ++ "/sets/" ++ p
