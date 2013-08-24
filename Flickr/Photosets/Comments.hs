--------------------------------------------------------------------
-- |
-- Module      : Flickr.Photosets.Comments
-- Description : flickr.photosets.comments - manage comments on photo sets.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer  : Sigbjorn Finne <sof@forkIO.com>
-- Stability   : provisional
-- Portability : portable
--
-- flickr.photosets.comments API, manage comments on photo sets.
--------------------------------------------------------------------
module Flickr.Photosets.Comments where

import Flickr.Monad
import Flickr.Types
import Flickr.Types.Import

-- | Add a comment to a photoset.
addComment :: PhotosetID -> String -> FM CommentID
addComment psid c = withWritePerm $ postMethod $
  flickTranslate toCommentID $ 
    flickrCall "flickr.photosets.comments.addComment"
               [ ("photoset_id", psid)
	       , ("comment_text", c)
	       ]

-- | Delete a photoset comment as the currently authenticated user.
deleteComment :: CommentID -> FM ()
deleteComment cid = withWritePerm $ postMethod $
    flickCall_ "flickr.photosets.comments.deleteComment"
               [ ("comment_id", cid)
	       ]

-- | Edit the text of a comment as the currently authenticated user.
editComment :: CommentID -> String -> FM ()
editComment cid c = withWritePerm $ postMethod $
    flickCall_ "flickr.photosets.comments.editComment"
               [ ("comment_id", cid)
	       , ("comment_text", c)
	       ]

-- | Returns the comments for a photoset.
getList :: PhotosetID -> FM [Comment]
getList psid = 
  flickTranslate toCommentList $ 
    flickrCall "flickr.photosets.comments.getList"
               [ ("photoset_id", psid)
	       ]


