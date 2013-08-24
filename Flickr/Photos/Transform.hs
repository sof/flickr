--------------------------------------------------------------------
-- |
-- Module      : Flickr.Photos.Transform
-- Description : flickr.photos.transform - rotation, mostly.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer  : Sigbjorn Finne <sof@forkIO.com>
-- Stability   : provisional
-- Portability : portable
--
-- flickr.photos.transform API, rotation only right now.
--------------------------------------------------------------------
module Flickr.Photos.Transform where

import Flickr.Monad
import Flickr.Types

-- | Rotate a photo.
rotate :: PhotoID -> Latitude -> FM ()
rotate pid d = withWritePerm $ postMethod $ 
  flickCall_ "flickr.photos.transform.rotate"
             [ ("photo_id", pid)
	     , ("degrees", d)
	     ]


