--------------------------------------------------------------------
-- |
-- Module      : Flickr.Photos.Licenses
-- Description : flickr.photos.licenses - setting/getting photo licensing.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer  : Sigbjorn Finne <sof@forkIO.com>
-- Stability   : provisional
-- Portability : portable
--
-- flickr.photos.licenses API, setting/getting photo licensing.
--------------------------------------------------------------------
module Flickr.Photos.Licenses where

import Flickr.Monad
import Flickr.Types
import Flickr.Types.Import

-- | Fetches a list of available photo licenses for Flickr.
getInfo :: FM [License]
getInfo = 
  flickTranslate toLicenseList $
   flickrCall "flickr.photos.licenses.getInfo"
              []

-- | Sets the license for a photo.
setLicense :: PhotoID -> LicenseID -> FM ()
setLicense pid lid = withWritePerm $ postMethod $
   flickCall_ "flickr.photos.licenses.setLicense"
              [ ("photo_id", pid)
	      , ("license_id", lid)
	      ]
