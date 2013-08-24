--------------------------------------------------------------------
-- |
-- Module      : Flickr.Photos.Geo
-- Description : flickr.photos.geo - setting/getting photo geo location.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer  : Sigbjorn Finne <sof@forkIO.com>
-- Stability   : provisional
-- Portability : portable
--
-- flickr.photos.geo API, setting/getting photo geo location.
--------------------------------------------------------------------
module Flickr.Photos.Geo 
       ( getLocation          -- :: PhotoID -> FM GeoLocation
       , removeLocation       -- :: PhotoID -> FM ()
       , setLocation          -- :: PhotoID -> GeoLocation -> FM ()
       , batchCorrectLocation -- :: GeoLocation -> Either PlaceID WhereOnEarthID -> FM ()
       , correctLocation      -- :: PhotoID -> Either PlaceID WhereOnEarthID -> FM ()
       , photosForLocation    -- :: GeoLocation -> [PhotoInfo] -> FM [Photo]
       
       , setContext     -- :: PhotoID -> ContextID -> FM ()

       , getPerms       -- :: PhotoID -> FM Permissions
       , setPerms       -- :: PhotoID -> Permissions -> FM ()
       
       ) where

import Flickr.Monad
import Flickr.Utils
import Flickr.Types
import Flickr.Types.Import

-- | Correct the places hierarchy for all the photos for a user at
-- a given latitude, longitude and accuracy. 
-- 
-- Batch corrections are processed in a delayed queue so it may take
-- a few minutes before the changes are reflected in a user's photos.
batchCorrectLocation :: GeoLocation -> Either PlaceID WhereOnEarthID -> FM ()
batchCorrectLocation (lat,lon,acc) ei = withWritePerm $ postMethod $ 
  flickCall_ "flickr.photos.geo.batchCorrectLocation"
             (eiArg "place_id" "woe_id" ei $
	      mbArg "accuracy" (fmap show acc) $
	      [ ("latitude",  lat)
	      , ("longitude", lon)
	      ])

-- | update/correct the location of attached to a photo.
correctLocation :: PhotoID -> Either PlaceID WhereOnEarthID -> FM ()
correctLocation pid ei = withWritePerm $ postMethod $ 
  flickCall_ "flickr.photos.geo.correctLocation"
             (eiArg "place_id" "woe_id" ei $
	      [ ("photo_id", pid) ])

-- | Get the geo data (latitude and longitude and the accuracy level) for a photo.
getLocation :: PhotoID -> FM GeoLocation
getLocation pid = 
  flickTranslate toGeoLocation $
    flickrCall "flickr.photos.geo.getLocation"
               [ ("photo_id", pid) ]

-- | Get permissions for who may view geo data for a photo.
getPerms :: PhotoID -> FM Permissions
getPerms pid = 
  flickTranslate toPermissions $
    flickrCall "flickr.photos.geo.getPerms"
               [ ("photo_id", pid) ]

-- | Return a list of photos for a user at a specific latitude, longitude and accuracy
photosForLocation :: GeoLocation -> [PhotoInfo] -> FM (PhotoContext, [Photo])
photosForLocation (lat,lon,acc) extras = withReadPerm $ 
  flickTranslate toPhotoList $
    flickrCall "flickr.photo.geo.photosForLocation"
               (mbArg "accuracy" (fmap show acc) $
	        lsArg "extras"   (map show extras) $
		[ ("latitude", lat)
		, ("longitude", lon)
		])

-- | Removes the geo data associated with a photo.
removeLocation :: PhotoID -> FM ()
removeLocation pid = withWritePerm $ postMethod $
    flickCall_ "flickr.photos.geo.removeLocation"
               [ ("photo_id", pid) ]

-- | Indicate the state of a photo's geotagginess beyond
-- latitude and longitude. Photos passed to this method must
-- already be geotagged (using the 'setLocation' method).
setContext :: PhotoID -> ContextID -> FM ()
setContext pid ctxtId = withWritePerm $ postMethod $ 
   flickCall_ "flickr.photos.geo.setContext"
              [ ("photo_id", pid)
	      , ("context",  show ctxtId)
	      ]

-- | Sets the geo data (latitude and longitude and, optionally,
-- the accuracy level) for a photo. Before users may assign
-- location data to a photo they must define who, by default,
-- may view that information. Users can edit this preference
-- at http://www.flickr.com/account/geo/privacy/. If a user
-- has not set this preference, the API method will return 
-- an error.
setLocation :: PhotoID -> GeoLocation -> FM ()
setLocation pid (la,lo,ac) = withWritePerm $ postMethod $ 
    flickCall_ "flickr.photos.geo.setLocation"
               (mbArg "accuracy" (fmap show ac) $
	         [ ("photo_id", pid)
	         , ("lat", la)
	         , ("lon", lo)
	         ])

-- | Set the permission for who may view the geo data associated with a photo.
setPerms :: PhotoID -> Permissions -> FM ()
setPerms pid p = withWritePerm $ postMethod $ 
    flickCall_ "flickr.photos.geo.setPerms"
               [ ("photo_id", pid)
	       , ("is_public",  showBool $ permIsPublic p)
	       , ("is_friend",  showBool $ permIsFriend p)
	       , ("is_family",  showBool $ permIsFamily p)
	       , ("is_contact", showBool (permIsFamily p || permIsFriend p))
	       ]



