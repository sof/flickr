--------------------------------------------------------------------
-- |
-- Module      : Flickr.Tags
-- Description : flickr.tags - fetch photos by tag or cluster membership.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- The flickr.tags API, fetching photos by tag or cluster membership.
--------------------------------------------------------------------
module Flickr.Tags 
       ( getClusterPhotos   -- :: TagID -> ClusterID -> FM (PhotoContext, [Photo])
       , getClusters        -- :: Tag -> FM [Cluster]
       , getHotList         -- :: Maybe DayWeek -> Maybe Int -> FM [TagDetails]
       , getListPhoto       -- :: PhotoID -> FM [TagDetails]
       , getListUser        -- :: Maybe UserID -> FM [TagDetails]
       , getListUserPopular -- :: Maybe UserID -> Maybe Int -> FM [TagDetails]
       , getListUserRaw     -- :: Maybe Tag -> FM [TagDetails]
       , getRelated         -- :: Tag -> FM [TagDetails]
       ) where

import Flickr.Monad
import Flickr.Types
import Flickr.Types.Import

-- | Returns the first 24 photos for a given tag cluster.
getClusterPhotos :: TagID -> ClusterID -> FM (PhotoContext, [Photo])
getClusterPhotos t c = 
   flickTranslate toPhotoList $
     flickrCall "flickr.tags.getClusterPhotos"
                [ ("tag", t)
		, ("cluster_id", c)
		]

-- | Gives you a list of tag clusters for the given tag.
getClusters :: Tag -> FM [Cluster]
getClusters t = 
  flickTranslate toClusterList $
    flickrCall "flickr.tags.getClusters"
               [ ("tag", t) ]

-- | Returns a list of hot tags for the given period.
getHotList :: Maybe DayWeek -> Maybe Int -> FM [TagDetails]
getHotList period count = 
  flickTranslate toTagDetailsList $
    flickrCall "flickr.tags.getHotList"
               (mbArg "period" (fmap (\ x -> if x then "day" else "week") period) $
	        mbArg "count"  (fmap show count) [])

-- | Get the tag list for a given photo.
getListPhoto :: PhotoID -> FM [TagDetails]
getListPhoto pid = 
  flickTranslate toTagDetailsList $
    flickrCall "flickr.tags.getListPhoto"
               [ ("photo_id", pid) ]

-- | Get the tag list for a given user (or the currently logged in user).
getListUser :: Maybe UserID -> FM [TagDetails]
getListUser mbid = 
  flickTranslate toTagDetailsList $
   flickrCall "flickr.tags.getListUser"
              (mbArg "user_id" mbid [])

-- | Get the popular tags for a given user (or the currently logged in user).
getListUserPopular :: Maybe UserID -> Maybe Int -> FM [TagDetails]
getListUserPopular uid c = 
  flickTranslate toTagDetailsList $
    flickrCall "flickr.tags.getListUserPopular"
               (mbArg "user_id" uid $
	        mbArg "count" (fmap show c) [])

-- | Get the raw versions of a given tag (or all tags) for the currently logged-in user.
getListUserRaw :: Maybe Tag -> FM [TagDetails]
getListUserRaw t = 
  flickTranslate toTagDetailsList $
   flickrCall "flickr.tags.getListUserRaw"
              (mbArg "tag" t [])

-- | Returns a list of tags 'related' to the given tag, based on clustered usage analysis.
getRelated :: Tag -> FM [TagDetails]
getRelated t = 
  flickTranslate toTagDetailsList $
   flickrCall "flickr.tags.getRelated"
              [ ("tag", t) ]

