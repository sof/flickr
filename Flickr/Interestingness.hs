--------------------------------------------------------------------
-- |
-- Module      : Flickr.Interestingness
-- Description : fetch ranked photos by Flickr interest level.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Fetch ranked photos by Flickr interest level.
--------------------------------------------------------------------
module Flickr.Interestingness where

import Flickr.Monad
import Flickr.Types
import Flickr.Types.Import

import Control.Monad

-- | Returns the list of interesting photos for the most recent day or a user-specified date.
getList :: Maybe DateString -> [PhotoInfo] -> FM [Photo]
getList d ps = liftM snd $
 flickTranslate toPhotoList $
  flickrCall "flickr.interestingness.getList"
             (mbArg "date" d $
	       lsArg "extras" (map show ps) [])
