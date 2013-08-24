--------------------------------------------------------------------
-- |
-- Module      : Flickr.Photosets
-- Description : flickr.photosets - navigating and managing sets.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer  : Sigbjorn Finne <sof@forkIO.com>
-- Stability   : provisional
-- Portability : portable
--
-- flickr.photosets API, navigating and managing photo sets.
--------------------------------------------------------------------
module Flickr.Photosets where

import Flickr.Monad
import Flickr.Types
import Flickr.Utils
import Flickr.Types.Import

import Data.List

-- | Add a photo to the end of an existing photoset.
addPhoto :: PhotosetID -> PhotoID -> FM ()
addPhoto psid pid = withWritePerm $ postMethod $
  flickCall_ "flickr.photosets.addPhoto"
             [ ("photoset_id", psid)
	     , ("photo_id", pid)
	     ]

-- | Create a new photoset for the calling user.
create :: String -> Maybe String -> PhotoID -> FM Photoset
create title mbDesc primPid = withWritePerm $ postMethod $
  flickTranslate toPhotoset $
   flickrCall "flickr.photosets.create"
              (mbArg "description" mbDesc $
                [ ("title", title)
	        , ("primary_photo_id", primPid)
	        ])

-- | Delete a photoset.
delete :: PhotosetID -> FM ()
delete psid = withWritePerm $ postMethod $
  flickCall_ "flickr.photosets.delete"
             [ ("photoset_id", psid)
	     ]

-- | Modify the meta-data for a photoset.
editMeta :: PhotosetID -> String -> Maybe String -> FM ()
editMeta psid title mbDesc = withWritePerm $ postMethod $
  flickCall_ "flickr.photosets.editMeta"
             (mbArg "description" mbDesc $
               [ ("photoset_id", psid)
	       , ("title", title)
	       ])

-- | Modify the photos in a photoset. Use this method to add, remove and re-order photos.
editPhotos :: PhotosetID -> PhotoID -> [PhotoID] -> FM ()
editPhotos psid primPhoto pids = withWritePerm $ postMethod $
  flickCall_ "flickr.photosets.editPhotos"
               [ ("photoset_id", psid)
	       , ("primary_photo_id", primPhoto)
	       , ("photo_ids", intercalate "," pids)
	       ]

-- | Returns next and previous photos for a photo in a set.
getContext :: PhotosetID -> PhotoID -> FM (Photo, Photo)
getContext psid pid = 
  flickTranslate toPhotoPair $
   flickrCall "flickr.photosets.getContext"
                [ ("photo_id", pid)
	        , ("photoset_id", psid)
	        ]

-- | Gets information about a photoset.
getInfo :: PhotosetID -> FM Photoset
getInfo psid = 
  flickTranslate toPhotoset $
   flickrCall "flickr.photosets.getInfo"
                [ ("photoset_id", psid)
	        ]

-- | Returns the photosets belonging to the specified user.
getList :: Maybe UserID -> FM (Bool, [Photoset])
getList mbUser = 
  flickTranslate toRes $ 
   flickrCall "flickr.photosets.getList"
              (mbArg "user_id" mbUser [])
 where
  toRes s = parseDoc eltRes s
  
  eltRes e = do
    u <- eltBool "cancreate" e
    ls <- mapM eltPhotoset (pNodes "photoset" (children e))
    return (u,ls)

-- | Get the list of photos in a set.
getPhotos :: PhotosetID
          -> [PhotoInfo]
	  -> Maybe Privacy
	  -> Maybe MediaType
	  -> FM Photoset
getPhotos psid extras priv med = 
 flickTranslate toPhotoset $
  flickrCall "flickr.photosets.getPhotos"
             (mbArg "privacy_filter" (fmap (show.fromEnum) priv) $
	      mbArg "media" (fmap show med) $ 
	      lsArg "extras" (map show extras)
	       [ ("photoset_id", psid) ])

-- | Set the order of photosets for the calling user.
orderSets :: [PhotosetID] -> FM ()
orderSets psids = withWritePerm $ postMethod $
  flickCall_ "flickr.photosets.orderSets"
               [ ("photoset_ids", intercalate "," psids)
	       ]

-- | Remove a photo from a photoset.
removePhoto :: PhotosetID -> PhotoID -> FM ()
removePhoto psid pid = withWritePerm $ postMethod $
  flickCall_ "flickr.photosets.removePhoto"
             [ ("photoset_id", psid)
	     , ("photo_id", pid)
	     ]



