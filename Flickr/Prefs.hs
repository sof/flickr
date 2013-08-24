--------------------------------------------------------------------
-- |
-- Module      : Flickr.Prefs
-- Description : flickr.prefs - accessing user preferences.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer  : Sigbjorn Finne <sof@forkIO.com>
-- Stability   : provisional
-- Portability : portable
--
-- flickr.prefs API, accessing a user's preferences.
--------------------------------------------------------------------
module Flickr.Prefs 
       ( getContentType -- :: FM ContentType
       , getGeoPerms    -- :: FM Privacy
       , getHidden      -- :: FM Bool
       , getPrivacy     -- :: FM Privacy
       , getSafetyLevel -- :: FM Int
       ) where

import Flickr.Monad
import Flickr.Types
import Flickr.Types.Import

-- | Returns the default content type preference for the user.
getContentType :: FM ContentType
getContentType = withReadPerm $
 flickTranslate toContentType $
  flickCall "flickr.prefs.getContentType" []

-- | Returns the default privacy level for geographic information 
-- attached to the user's photos. 
getGeoPerms :: FM Privacy
getGeoPerms = withReadPerm $
  flickTranslate (toPrivacy "geoperms") $
   flickCall "flickr.prefs.getGeoPerms" []

-- | Returns the default hidden preference for the user.
getHidden :: FM Bool
getHidden = withReadPerm $
  flickTranslate (toBool "hidden") $
   flickCall "flickr.prefs.getHidden" []

-- | Returns the default privacy level preference for the user.
getPrivacy :: FM Privacy
getPrivacy = withReadPerm $
  flickTranslate (toPrivacy "privacy") $ 
   flickCall "flickr.prefs.getPrivacy" []

-- | Returns the default safety level preference for the user.
getSafetyLevel :: FM Int
getSafetyLevel = withReadPerm $
  flickTranslate (toSafetyLevel "safety_level") $
   flickCall "flickr.prefs.getSafetyLevel" []

