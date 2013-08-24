--------------------------------------------------------------------
-- |
-- Module      : Flickr.Places
-- Description : flickr.places - geo locating photos.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer  : Sigbjorn Finne <sof@forkIO.com>
-- Stability   : provisional
-- Portability : portable
--
-- flickr.places API, locating photos by places and geo.
--------------------------------------------------------------------
module Flickr.Places 
       ( find                        -- :: String -> FM (PlaceQuery, [Place])
       , findByLatLon                -- :: Latitude -> Longitude -> Maybe Accuracy -> FM (PlaceQuery, [Place)]
       , getChildrenWithPhotosPublic -- :: Either PlaceID WhereOnEarthID -> FM (PlaceQuery, [Place])
       , getInfo              -- :: Either PlaceID WhereOnEarthID -> FM LocationPlace
       , getInfoByUrl         -- :: URLString -> FM LocationPlace
       , getPlaceTypes        -- :: FM [PlaceType]
       
       , placesForBoundingBox -- :: BoundingBox -> Maybe PlaceTypeName -> Maybe PlaceTypeId -> FM [Place]
       , placesForContacts    -- :: PlaceType -> Maybe WhereOnEarthID
                              -- -> Maybe PlaceTypeId -> Maybe Threshold
			      -- -> Maybe String -> FM [Place]
       , placesForTags   -- :: PlaceType
                         -- -> Maybe WhereOnEarthID
	                 -- -> Maybe PlaceTypeId
	                 -- -> Maybe Threshold
	                 -- -> Maybe [Tag]
	                 -- -> Maybe String -- tag-mode
	                 -- -> Maybe MachineTag
	                 -- -> FM [Place]
       , placesForUser   -- :: PlaceTypeName
                         -- -> Maybe WhereOnEarthID
	                 -- -> Maybe PlaceTypeId
	                 -- -> Maybe Threshold
	                 -- -> FM [Place]
       , resolvePlaceId  -- :: PlaceID -> FM LocationPlace
       , resolvePlaceURL -- :: URLString -> FM LocationPlace
       
       , tagsForPlace    -- :: Either PlaceID WhereOnEarthID -> DateDetails -> FM [TagInfo]
       ) where

import Flickr.Monad
import Flickr.Types
import Flickr.Types.Import

import Data.List ( intercalate )

-- | Return a list of place IDs for a query string.
-- The flickr.places.find method is not a geocoder. 
-- It will round up to the nearest place type to 
-- which place IDs apply. For example, if you pass 
-- it a street level address it will return the city 
-- that contains the address rather than the street, 
-- or building, itself.
find :: String -> FM (PlaceQuery, [Place])
find q = 
  flickTranslate toPlaces $ 
    flickrCall "flickr.places.find"
               [("query", q)]

-- | Return a place ID for a latitude, longitude and accuracy triple.
-- 
-- The flickr.places.findByLatLon method is not meant to be 
-- a (reverse) geocoder in the traditional sense. It is designed
-- to allow users to find photos for "places" and will round
-- up to the nearest place type to which corresponding place IDs 
-- apply.
-- 
-- For example, if you pass it a street level coordinate it will 
-- return the city that contains the point rather than the street, 
-- or building, itself.
-- 
-- It will also truncate latitudes and longitudes to three 
-- decimal points.
findByLatLon :: Latitude -> Longitude -> Maybe Accuracy -> FM (PlaceQuery, [Place])
findByLatLon la lon acc = 
  flickTranslate toPlaces $ 
    flickCall "flickr.places.findByLatLon" 
              (mbArg "accuracy" (fmap show acc) $
	         [("lat", la),("lon", lon)])

-- | Return a list of locations with public photos that are parented by a Where on Earth (WOE) or Places ID.
getChildrenWithPhotosPublic :: Either PlaceID WhereOnEarthID
                            -> FM (PlaceQuery, [Place])
getChildrenWithPhotosPublic pw = 
  flickTranslate toPlaces $
    flickrCall "flickr.places.getChildrenWithPhotosPublic"
               (eiArg "place_id" "woe_id" pw [])

-- | Lookup information about a place, by its flickr.com/places URL.
getInfoByUrl :: URLString -> FM LocationPlace
getInfoByUrl url = 
  flickTranslate toLocationPlace $
    flickrCall "flickr.places.getInfoByUrl"
               [ ("url", url) ]

-- | Get informations about a place.
getInfo :: Either PlaceID WhereOnEarthID -> FM LocationPlace
getInfo pw = 
  flickTranslate toLocationPlace $
    flickrCall "flickr.places.getInfo"
               (eiArg "place_id" "woe_id" pw [])

-- | Fetches a list of available place types for Flickr.
getPlaceTypes :: FM [PlaceType]
getPlaceTypes = 
  flickTranslate toPlaceTypes $
     flickrCall "flickr.places.getPlaceTypes" []

-- | return all the locations of a matching place type for a bounding box.
placesForBoundingBox :: BoundingBox
                     -> Maybe PlaceTypeName
		     -> Maybe PlaceTypeId
		     -> FM [Place]
placesForBoundingBox bbox pn pid = 
 flickTranslate toPlacesList $
   flickCall "flickr.places.placesForBoundingBox" 
             (mbArg "place_type" pn  $
	      mbArg "place_type_id" pid   $
  	        [("bbox", intercalate "," $ map show $ 
		            [ bboxMinLongitude bbox
			    , bboxMinLatitude  bbox
			    , bboxMaxLongitude bbox
			    , bboxMaxLatitude  bbox
			    ])])

-- | Return a list of the top 100 unique places clustered by a given placetype for a user's contacts. 
placesForContacts :: Either PlaceTypeName WhereOnEarthID
	      -> Maybe PlaceTypeId
	      -> Maybe Threshold
	      -> Maybe String
	      -> FM [Place]
placesForContacts eiPW pid th contacts = withReadPerm $
 flickTranslate toPlacesList $
   flickCall "flickr.places.placesForContacts" 
             (mbArg "woe_id" woe_id  $ 
	      mbArg "place_type" pnm   $
	      mbArg "place_type_id" pid   $
	      mbArg "threshold" (fmap show th) $
	      mbArg "contacts" contacts $
 	       [])
 where
   pnm    = either Just (const Nothing) eiPW
   woe_id = either (const Nothing) Just eiPW

-- | Return a list of the top 100 unique places clustered by a given placetype for a user. 
placesForUser :: Either PlaceID   WhereOnEarthID
	      -> Either PlaceTypeName PlaceTypeId
	      -> Maybe Threshold
	      -> DateDetails
	      -> FM [Place]
placesForUser eiPW eiPP th mbDates = withReadPerm $
 flickTranslate toPlacesList $
   flickCall "flickr.places.placesForUser" 
             (mbArg "place_id" place_id   $ 
	      mbArg "woe_id"   woe_id     $ 
	      mbArg "place_type_id" ptid  $
	      mbArg "place_type"    pt    $
	      mbArg "threshold" (fmap show th) $
	      mbArg "min_taken_date" (dateMinTaken mbDates) $
	      mbArg "max_taken_date" (dateMaxTaken mbDates) $
	      mbArg "min_upload_date" (dateMinUpload mbDates) $
	      mbArg "max_upload_date" (dateMaxUpload mbDates) 
                [])
 where
   place_id = either Just (const Nothing) eiPW
   woe_id   = either (const Nothing) Just eiPW
   ptid     = either Just (const Nothing) eiPP
   pt       = either (const Nothing) Just eiPP

-- | Return a list of the top 100 unique places clustered by a given placetype for set of tags or machine tags. 
placesForTags :: Either PlaceTypeName WhereOnEarthID
	      -> Maybe PlaceID
	      -> Maybe Threshold
	      -> Maybe [Tag]
	      -> Maybe TagMode -- tag-mode
	      -> Maybe [MachineTag]
	      -> Maybe TagMode
	      -> DateDetails
	      -> FM [Place]
placesForTags eiPW pid th tags tag_mode mt mt_mode mbDates = withReadPerm $
 flickTranslate toPlacesList $
   flickCall "flickr.places.placesForTags" 
             (mbArg "woe_id" woe_id  $ 
	      mbArg "place_type" pnm   $
	      mbArg "place_type_id" pid   $
	      mbArg "threshold" (fmap show th) $
	      mbArg "tags" (fmap (intercalate ",") tags) $
	      mbArg "tag_mode" tag_mode $
	      mbArg "machine_tags" (fmap (intercalate ",") $ fmap (map fromMT) mt) $ 
	      mbArg "machine_tag_mode" mt_mode $
	      mbArg "min_taken_date" (dateMinTaken mbDates) $
	      mbArg "max_taken_date" (dateMaxTaken mbDates) $
	      mbArg "min_upload_date" (dateMinUpload mbDates) $
	      mbArg "max_upload_date" (dateMaxUpload mbDates) 
 	       [])
 where
   pnm     = either Just (const Nothing) eiPW
   woe_id  = either (const Nothing) Just eiPW

fromMT :: MachineTag -> [Char]
fromMT mt = 
  case mTagValue mt of
   "" -> case (mTagNamespace mt, mTagPredicate mt) of
           ("","") -> ""
	   (xs,"") -> xs ++ ":"
	   ("",xs) -> "*:" ++ xs
	   (as,bs) -> as ++ ':':bs
   t  -> toS (mTagNamespace mt) ++ ':':toS (mTagPredicate mt) ++ '=':t
 where
  toS "" = "*"
  toS x  = x

-- | Find Flickr Places information by Place Id.
resolvePlaceId :: PlaceID -> FM LocationPlace
resolvePlaceId pid = 
  flickTranslate toLocationPlace $
   flickrCall "flickr.places.resolvePlaceId" 
              [("place_id", pid)]

-- | Find Flickr Places information by Place URL.
resolvePlaceURL :: URLString -> FM LocationPlace
resolvePlaceURL purl = 
  flickTranslate toLocationPlace $
    flickrCall "flickr.places.resolvePlaceURL" 
               [("url", purl)]

-- | Return a list of the top 100 unique tags for a 
-- Flickr Places or Where on Earth (WOE) ID.
tagsForPlace :: Either PlaceID WhereOnEarthID -> DateDetails -> FM [TagInfo]
tagsForPlace loc mbDates = 
  flickTranslate toTagInfoList $
    flickrCall "flickr.places.tagsForPlace"
               (mbArg "woe_id"   mbw $
	        mbArg "place_id" mbp $
		mbArg "min_taken_date" (dateMinTaken mbDates) $
		mbArg "max_taken_date" (dateMaxTaken mbDates) $
		mbArg "min_upload_date" (dateMinUpload mbDates) $
		mbArg "max_upload_date" (dateMaxUpload mbDates) 
		 [])
 where
   mbp  = either (const Nothing) Just loc
   mbw  = either Just (const Nothing) loc
  
