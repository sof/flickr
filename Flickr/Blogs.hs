--------------------------------------------------------------------
-- |
-- Module      : Flickr.Blogs
-- Description : flickr.blogs - post to photo blog.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- flickr.blogs API, accessing a user's blogs + post photos to them.
--------------------------------------------------------------------
module Flickr.Blogs where

import Flickr.Monad
import Flickr.Types
import Flickr.Types.Import

-- | Get a list of configured blogs for the calling user.
getList :: FM [Blog]
getList = withReadPerm $
  flickTranslate toBlogs $ 
    flickCall "flickr.blogs.getList" []

-- | Post a photo to the given blog.
postPhoto :: BlogID
          -> PhotoID -- ^ photo id
	  -> String  -- ^ title
	  -> String  -- ^ description
	  -> Maybe String -- ^ password
          -> FM ()
postPhoto bid pid tit desc pwd = withWritePerm $ postMethod $ 
   flickCall_ "flickr.blogs.postPhoto"
              (mbArg "blog_password" pwd
	         [ ("blog_id", bid)
		 , ("photo_id", pid)
		 , ("title", tit)
		 , ("description", desc)
		 ]) 

