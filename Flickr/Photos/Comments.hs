--------------------------------------------------------------------
-- |
-- Module      : Flickr.Photos.Comments
-- Description : flickr.photos.comments - setting/getting photo comments.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer  : Sigbjorn Finne <sof@forkIO.com>
-- Stability   : provisional
-- Portability : portable
--
-- flickr.photos.comments API, setting/getting photo comments.
--------------------------------------------------------------------
module Flickr.Photos.Comments where

import Flickr.Monad
import Flickr.Types
import Flickr.Types.Import

-- | Add comment to a photo as the currently authenticated user.
addComment :: PhotoID -> String -> FM CommentID
addComment pid comm = withWritePerm $ postMethod $
  flickTranslate toCommentID $ 
    flickrCall "flickr.photos.comments.addComment"
               [ ("photo_id", pid)
	       , ("comment_text", comm)
	       ]

-- | Delete a comment as the currently authenticated user.
deleteComment :: CommentID -> FM ()
deleteComment cid = withWritePerm $ postMethod $
    flickCall_ "flickr.photos.comments.deleteComment"
               [ ("comment_id", cid)
	       ]

-- | Edit the text of a comment as the currently authenticated user.
editComment :: CommentID -> String -> FM ()
editComment cid comm = withWritePerm $ postMethod $
    flickCall_ "flickr.photos.comments.editComment"
               [ ("comment_id", cid)
	       , ("comment_text", comm)
	       ]

-- | Returns the comments for a photo.
getList :: PhotoID -> DateDetails -> FM [Comment]
getList pid mbDates = 
  flickTranslate toCommentList $ 
    flickrCall "flickr.photos.comments.getList"
               (mbArg "min_comment_date" (dateMinTaken mbDates) $ 
	        mbArg "max_comment_date" (dateMaxTaken mbDates) $
	         [ ("photo_id", pid)
	         ])





