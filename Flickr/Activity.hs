--------------------------------------------------------------------
-- |
-- Module      : Flickr.Activity
-- Description : flickr.activity - photo comments and activity.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer  : Sigbjorn Finne <sof@forkIO.com>
-- Stability   : provisional
-- Portability : portable
--
-- flickr.activity, methods for accessing comments and activity for a user.
--------------------------------------------------------------------
module Flickr.Activity where

import Flickr.Monad
import Flickr.Types
import Flickr.Types.Import

import Data.Maybe ( maybeToList )

-- | Returns a list of recent activity on photos commented on by the calling user.
userComments :: FM [Item]
userComments = withReadPerm $ 
  flickTranslate toItems $ flickCall "flickr.activity.userComments" []

-- | Returns a list of recent activity on photos belonging to the calling user.
userPhotos :: Maybe Integer -- time frame
	   -> FM [Item]
userPhotos mb = withReadPerm $
  flickTranslate toItems $ 
    flickCall "flickr.activity.userPhotos" 
              (maybeToList (fmap (\x -> ("timeframe",show x)) mb))

	   

