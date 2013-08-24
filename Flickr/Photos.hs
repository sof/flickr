--------------------------------------------------------------------
-- |
-- Module      : Flickr.Photos
-- Description : flickr.photos - manage and access user photos.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer  : Sigbjorn Finne <sof@forkIO.com>
-- Stability   : provisional
-- Portability : portable
--
-- flickr.photos API, searching, managing and access a user's photos.
-- <http://www.flickr.com/services/api/>
--------------------------------------------------------------------
module Flickr.Photos where

import Flickr.Monad
import Flickr.Types
import Flickr.Types.Import

import Data.Maybe
import Data.List
import Flickr.Utils
import Text.XML.Light.Proc   ( findChildren )
import Control.Monad

-- | Add tags to a photo.
addTags :: PhotoID -> [Tag] -> FM ()
addTags pid tgs = withWritePerm $ postMethod $ 
  flickCall_ "flickr.photos.addTags"
             (lsArg "tags" tgs 
	            [ ("photo_id", pid) ])

-- | Delete a photo from flickr.
delete :: PhotoID -> FM ()
delete pid = withDeletePerm $ postMethod $ 
  flickCall_ "flickr.photos.delete"
             [ ("photo_id", pid) ]

-- | Returns all visible sets and pools the photo belongs to.
getAllContexts :: PhotoID -> FM ([Photoset],[PhotoPool])
getAllContexts pid = 
  flickTranslate toResList $
    flickrCall "flickr.photos.getAllContexts"
               [ ("photo_id", pid) ]
 where  
  toResList s = parseDoc eltRes s
  
  eltRes e = do
    let ss = mapMaybe eltPhotoset  $ findChildren (nsName "set") e
    let ps = mapMaybe eltPhotoPool $ findChildren (nsName "pool") e
    return (ss, ps)

-- | Fetch a list of recent photos from the calling users' contacts.
getContactsPhotos :: Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> [PhotoInfo] -> FM [Photo]
getContactsPhotos mbCount just_friends single_photo include_self extras = liftM snd $
  flickTranslate toPhotoList $
    flickrCall "flickr.photos.getContactsPhotos"
               (mbArg "count" (fmap show mbCount) $
	         mbArg "just_friends" (fmap showBool just_friends) $
		  mbArg "single_photo" (fmap showBool single_photo) $
 		   mbArg "include_self" (fmap showBool include_self) $
		    lsArg "extras" (map show extras) [])

-- | Fetch a list of recent public photos from a users' contacts.
getContactsPublicPhotos :: Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> [PhotoInfo] -> FM [Photo]
getContactsPublicPhotos mbCount just_friends single_photo include_self extras  = liftM snd $
  flickTranslate toPhotoList $
    flickrCall "flickr.photos.getContactsPublicPhotos"
               (mbArg "count" (fmap show mbCount) $
	         mbArg "just_friends" (fmap showBool just_friends) $
		  mbArg "single_photo" (fmap showBool single_photo) $
 		   mbArg "include_self" (fmap showBool include_self) $
		    lsArg "extras" (map show extras) [])

-- | Fetch a list of recent photos from the calling users' contacts.
getContext :: PhotoID -> FM (Photo,Photo)
getContext pid = 
  flickTranslate toPhotoPair $
    flickrCall "flickr.photos.getContext"
               [ ("photo_id", pid) ]

-- | Gets a list of photo counts for the given date ranges for the calling user.
getCounts :: [DateString] -> [DateString] -> FM [PhotoCount]
getCounts unix_ts sql_ts = 
  flickTranslate toPhotoCountList $
    flickrCall "flickr.photos.getCounts"
               (lsArg "dates"  unix_ts $
	         lsArg "taken_dates" sql_ts [])

-- | Retrieves a list of EXIF/TIFF/GPS tags for a given photo. The calling user 
-- must have permission to view the photo.
getExif :: PhotoID -> Maybe String -> FM [EXIF]
getExif pid secret = 
  flickTranslate toEXIFList $
    flickrCall "flickr.photos.getExif"
               (mbArg "secret"  secret 
	         [ ("photo_id", pid) ])

-- | Returns the list of people who have favorited a given photo.
getFavorites :: PhotoID -> FM [(User,Date)]
getFavorites pid = 
  flickTranslate toResList $
    flickrCall "flickr.photos.getFavorites"
	         [ ("photo_id", pid) ]
 where  
  toResList s = parseDoc eltRes s
  
  eltRes e = do
    let es = findChildren (nsName "person") e
    mapM ( \ p -> do
        fd <- pAttr "favedate" p
	u  <- eltUser p
	return (u,fd)) es

-- | Get information about a photo. The calling user must have permission to view the photo.
getInfo :: PhotoID -> Maybe String -> FM PhotoDetails
getInfo pid secret = 
  flickTranslate toPhotoDetails $
    flickrCall "flickr.photos.getInfo"
               (mbArg "secret"  secret 
	         [ ("photo_id", pid) ])

-- | Returns a list of your photos that are not part of any sets.
getNotInSet :: Maybe DateInterval
            -> Maybe DateInterval
	    -> Maybe Privacy
	    -> Maybe MediaType
	    -> [PhotoInfo]
	    -> FM [Photo]
getNotInSet mbUpload mbTaken priv med extras = liftM snd $
  flickTranslate toPhotoList $
    flickrCall "flickr.photos.getNotInSet"
               (mbArg "min_upload_date"  mbUpload1 $
	         mbArg "max_upload_date" mbUpload2 $
		  mbArg "min_taken_date" mbTaken1  $
 		   mbArg "max_taken_date" mbTaken2 $
		    mbArg "privacy_filter" (fmap (show.fromEnum) priv) $
		     mbArg "media" (fmap show med) $
		      lsArg "extras" (map show extras) [])
 where
  mbUpload1 = fmap fst mbUpload
  mbUpload2 = mbUpload >>= \ x -> snd x

  mbTaken1 = fmap fst mbTaken
  mbTaken2 = mbTaken >>= \ x -> snd x
  


-- | Get permissions for a photo.
getPerms :: PhotoID -> FM Permissions
getPerms pid = withReadPerm $
  flickTranslate toPermissions $
    flickrCall "flickr.photos.getPerms"
	       [ ("photo_id", pid) ]

-- | Returns a list of the latest public photos uploaded to flickr.
getRecent :: [PhotoInfo] -> FM [Photo]
getRecent extras = liftM snd $
  flickTranslate toPhotoList $
    flickrCall "flickr.photos.getRecent"
	       (lsArg "extras" (map show extras) [])

-- | Returns the available sizes for a photo. The calling user must have permission to view the photo.
getSizes :: PhotoID -> FM [SizeDetails]
getSizes pid = 
  flickTranslate toSizeList $
    flickrCall "flickr.photos.getSizes"
	       [ ("photo_id", pid) ]

-- | Returns a list of your photos with no tags.
getUntagged :: Maybe DateInterval
            -> Maybe DateInterval
	    -> Maybe Privacy
	    -> Maybe MediaType
	    -> [PhotoInfo]
	    -> FM [Photo]
getUntagged mbUpload mbTaken priv med extras = liftM snd $
  flickTranslate toPhotoList $
    flickrCall "flickr.photos.getUntagged"
               (mbArg "min_upload_date"  mbUpload1 $
	         mbArg "max_upload_date" mbUpload2 $
		  mbArg "min_taken_date" mbTaken1  $
 		   mbArg "max_taken_date" mbTaken2 $
		    mbArg "privacy_filter" (fmap (show.fromEnum) priv) $
		     mbArg "media" (fmap show med) $
		      lsArg "extras" (map show extras) [])
 where
  mbUpload1 = fmap fst mbUpload
  mbUpload2 = mbUpload >>= \ x -> snd x

  mbTaken1 = fmap fst mbTaken
  mbTaken2 = mbTaken >>= \ x -> snd x
  
-- | Returns a list of your geo-tagged photos.
getWithGeoData :: Maybe DateInterval
	       -> Maybe DateInterval
	       -> Maybe Privacy
	       -> Maybe SortKey
	       -> Maybe MediaType
	       -> [PhotoInfo]
	       -> FM [Photo]
getWithGeoData mbUpload mbTaken priv sortKey med extras = liftM snd $
  flickTranslate toPhotoList $
    flickrCall "flickr.photos.getWithGeoData"
               (mbArg "min_upload_date"  mbUpload1 $
	         mbArg "max_upload_date" mbUpload2 $
		  mbArg "min_taken_date" mbTaken1  $
 		   mbArg "max_taken_date" mbTaken2 $
		    mbArg "privacy_filter" (fmap (show.fromEnum) priv) $
		     mbArg "sort" (fmap show sortKey) $
 		      mbArg "media" (fmap show med) $
		       lsArg "extras" (map show extras) [])
 where
  mbUpload1 = fmap fst mbUpload
  mbUpload2 = mbUpload >>= \ x -> snd x

  mbTaken1 = fmap fst mbTaken
  mbTaken2 = mbTaken >>= \ x -> snd x

-- | Returns a list of your photos which haven't been geo-tagged.
getWithoutGeoData :: Maybe DateInterval
	          -> Maybe DateInterval
	          -> Maybe Privacy
	          -> Maybe SortKey
	          -> Maybe MediaType
	          -> [PhotoInfo]
	          -> FM [Photo]
getWithoutGeoData mbUpload mbTaken priv sortKey med extras = liftM snd $
  flickTranslate toPhotoList $
    flickrCall "flickr.photos.getWithoutGeoData"
               (mbArg "min_upload_date"  mbUpload1 $
	         mbArg "max_upload_date" mbUpload2 $
		  mbArg "min_taken_date" mbTaken1  $
 		   mbArg "max_taken_date" mbTaken2 $
		    mbArg "privacy_filter" (fmap (show.fromEnum) priv) $
		     mbArg "sort" (fmap show sortKey) $
 		      mbArg "media" (fmap show med) $
		       lsArg "extras" (map show extras) [])
 where
  mbUpload1 = fmap fst mbUpload
  mbUpload2 = mbUpload >>= \ x -> snd x

  mbTaken1 = fmap fst mbTaken
  mbTaken2 = mbTaken >>= \ x -> snd x


-- | Return a list of your photos that have been recently created or which have 
-- been recently modified. Recently modified may mean that the photo's metadata (title, 
-- description, tags) may have been changed or a comment has been 
-- added (or just modified somehow :-)
recentlyUpdated :: DateString -> [PhotoInfo] -> FM (PhotoContext, [Photo])
recentlyUpdated minDate extras = withReadPerm $
  flickTranslate toPhotoList $
    flickrCall "flickr.photos.recentlyUpdated"
               (lsArg "extras" (map show extras) 
	         [ ("min_date", minDate) ])

-- | Remove a tag from a photo.
removeTag :: Tag -> FM ()
removeTag tag = withWritePerm $ postMethod $ 
    flickCall_ "flickr.photos.removeTag"
	       [ ("tag_id", tag) ]

-- | Return a list of photos matching some criteria. Only photos 
-- visible to the calling user will be returned. To return private 
-- or semi-private photos, the caller must be authenticated 
-- with 'read' permissions, and have permission to view the 
-- photos. Unauthenticated calls will only return public photos.
search :: Maybe UserID
       -> SearchConstraints
       -> [PhotoInfo]
       -> FM (PhotoContext, [Photo])
search uid sc extras = 
  flickTranslate toPhotoList $
    flickrCall "flickr.photos.search"
               (mbArg "user_id" uid $
	         lsArg "tags" (s_tags sc) $
		 mbArg "tag_mode" (fmap (\ x -> if x then "all" else "any") (s_tag_mode sc)) $
		   mbArg "text" (s_text sc) $
		    mbArg "min_upload_date" mbUpload1 $
		    mbArg "max_upload_date" mbUpload2 $
		    mbArg "min_taken_date" mbTaken1 $
		    mbArg "max_taken_date" mbTaken2 $
		    mbArg "license" (fmap (intercalate ",") (s_license sc)) $
		    mbArg "sort" (fmap show $ s_sort sc) $
		    mbArg "privacy_filter" (fmap (show.fromEnum) $ s_privacy sc) $
		    mbArg "bbox" (fmap show $ s_bbox sc) $
		    mbArg "accuracy" (fmap show (s_accuracy sc)) $
		    mbArg "safe_search" (fmap (show.succ.fromEnum) (s_safe_search sc)) $
		    mbArg "content_type" (fmap (show.succ.fromEnum) (s_content_type sc)) $
		    lsArg "machine_tags" (s_machine_tags sc) $
		    mbArg "machine_tag_mode" (fmap (\ x -> if x then "all" else "any") (s_machine_tag_mode sc)) $
		    mbArg "group_id" (s_group_id sc) $
		    mbArg "contacts" (fmap (\ x -> if x then "all" else "ff") (s_contacts sc)) $
		    mbArg "woe_id" (fmap show (s_woe_id sc)) $
		    mbArg "place_id" (s_place_id sc) $
		    mbArg "media"  (fmap show (s_media sc)) $
		    mbArg "has_geo" (fmap showBool (s_has_geo sc)) $
		    mbArg "lat" (s_lat sc) $
		    mbArg "lon" (s_lon sc) $
		    mbArg "radius" (s_radius sc) $
		    mbArg "radius_units" (s_radius_units sc) $
		    mbArg "is_commons"   (fmap showBool (s_is_commons sc)) $
		    lsArg "extras" (map show extras) [])
 where
  mbUpload1 = fmap fst (s_upload sc)
  mbUpload2 = (s_upload sc) >>= \ x -> snd x

  mbTaken1 = fmap fst (s_taken sc)
  mbTaken2 = (s_taken sc) >>= \ x -> snd x

data SearchConstraints
 = SearchConstraints
     { s_tags         :: [Tag]
     , s_tag_mode     :: Maybe Bool
     , s_text         :: Maybe String
     , s_upload       :: Maybe DateInterval
     , s_taken        :: Maybe DateInterval
     , s_license      :: Maybe [LicenseID]
     , s_sort         :: Maybe SortKey
     , s_privacy      :: Maybe Privacy
     , s_bbox         :: Maybe BoundingBox
     , s_accuracy     :: Maybe Accuracy
     , s_safe_search  :: Maybe Safety
     , s_content_type :: Maybe ContentType
     , s_machine_tags :: [Tag]
     , s_machine_tag_mode :: Maybe Bool
     , s_group_id     :: Maybe GroupID
     , s_contacts     :: Maybe Bool
     , s_woe_id       :: Maybe WhereOnEarthID
     , s_place_id     :: Maybe PlaceID
     , s_media        :: Maybe MediaType
     , s_has_geo      :: Maybe Bool
     , s_lat          :: Maybe Decimal
     , s_lon          :: Maybe Decimal
     , s_radius       :: Maybe Decimal
     , s_radius_units :: Maybe String
     , s_is_commons   :: Maybe Bool
     }

nullSearchConstraints :: SearchConstraints
nullSearchConstraints = SearchConstraints
     { s_tags      = []
     , s_tag_mode  = Nothing
     , s_text      = Nothing
     , s_upload    = Nothing
     , s_taken     = Nothing
     , s_license   = Nothing
     , s_sort      = Nothing
     , s_privacy   = Nothing
     , s_bbox      = Nothing
     , s_accuracy  = Nothing
     , s_safe_search  = Nothing
     , s_content_type = Nothing
     , s_machine_tags = []
     , s_machine_tag_mode = Nothing
     , s_group_id = Nothing
     , s_contacts = Nothing
     , s_woe_id   = Nothing
     , s_place_id = Nothing
     , s_media    = Nothing
     , s_has_geo  = Nothing
     , s_lat      = Nothing
     , s_lon      = Nothing
     , s_radius   = Nothing
     , s_radius_units = Nothing
     , s_is_commons = Nothing
     }

-- | Set the content type of a photo.
setContentType :: PhotoID -> ContentType -> FM ()
setContentType pid c = withWritePerm $ postMethod $ 
    flickCall_ "flickr.photos.setContentType"
	       [ ("photo_id", pid)
	       , ("content_type", showContentType c)
	       ]

-- | Set one or both of the dates for a photo.
setDates :: PhotoID -> Maybe DateString -> Maybe DateString -> Maybe DateGranularity -> FM ()
setDates pid datePosted dateTaken dateTakenGranularity = withWritePerm $ postMethod $ 
    flickCall_ "flickr.photos.setDates"
	       (mbArg "date_posted" datePosted $
	         mbArg "date_taken" dateTaken $
		  mbArg "date_taken_granularity" (fmap show dateTakenGranularity) $
		    [ ("photo_id", pid) ])

-- | Set the meta information for a photo.
setMeta :: PhotoID -> Title -> Description -> FM ()
setMeta pid title desc = withWritePerm $ postMethod $ 
    flickCall_ "flickr.photos.setMeta"
	       [ ("photo_id", pid)
	       , ("title", title)
	       , ("description", desc)
	       ]

-- | Set permissions for a photo.
setPerms :: PhotoID -> Permissions -> FM ()
setPerms pid p = withWritePerm $ postMethod $ 
    flickCall_ "flickr.photos.setPerms"
               [ ("photo_id", pid)
	       , ("is_public", showBool (permIsPublic p))
	       , ("is_friend", showBool (permIsFriend p))
	       , ("is_family", showBool (permIsFamily p))
	       , ("perm_comment", show (permCommentLevel p))
	       , ("perm_addmeta", show (permAddMetaLevel p))
	       ]

-- | Set the safety level of a photo.
setSafetyLevel :: PhotoID -> Maybe Safety -> Maybe Bool -> FM ()
setSafetyLevel pid mbSaf mbHid = withWritePerm $ postMethod $ 
    flickCall_ "flickr.photos.setSafetyLevel"
	       (mbArg "safety_level" (fmap showSafety mbSaf) $
	         mbArg "hidden" (fmap showBool mbHid) $
		    [ ("photo_id", pid) ])

-- | Set the tags for a photo.
setTags :: PhotoID -> [Tag] -> FM ()
setTags pid ts = withWritePerm $ postMethod $ 
    flickCall_ "flickr.photos.setTags"
	       (lsArg "tags" ts $
		    [ ("photo_id", pid)])

-- | locate the URL for the photo..local, non-Flickr, helper function.
-- Returns '<unknown>' if cannot be located.
getPhotoURL :: PhotoDetails -> URLString
getPhotoURL p = 
   case fromMaybe "" (photoURL (photoDetailsPhoto p)) of
     "" -> case photoDetailsURLs p of
             [] -> "<unknown>"
	     (u:_) -> urlDetailsURL u
     us -> us
  
