--------------------------------------------------------------------
-- |
-- Module      : Flickr.Photos.Notes
-- Description : flickr.photos.notes - manage photo annotations.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer  : Sigbjorn Finne <sof@forkIO.com>
-- Stability   : provisional
-- Portability : portable
--
-- flickr.photos.notes API, managing a user's photo annotations.
--------------------------------------------------------------------
module Flickr.Photos.Notes where

import Flickr.Monad
import Flickr.Types
import Flickr.Types.Import

-- | Add a note to a photo. Coordinates and sizes are in 
-- pixels, based on the 500px image size shown on individual 
-- photo pages.
add :: PhotoID -> Point -> Size -> String -> FM NoteID
add pid xy wh txt = withWritePerm $ postMethod $
  flickTranslate toNoteID $
    flickrCall "flickr.photos.notes.add"
               [ ("photo_id", pid)
	       , ("note_x",   show $ pointX xy)
	       , ("note_y",   show $ pointY xy)
	       , ("note_w",   show $ sizeW  wh)
	       , ("note_h",   show $ sizeH  wh)
	       , ("note_text", txt)
	       ]


-- | Delete a note from a photo.
delete :: NoteID -> FM ()
delete nid = withWritePerm $ postMethod $ 
  flickCall_ "flickr.photos.notes.delete"
             [ ("note_id", nid) ]

-- | Edit a note on a photo. Coordinates and sizes are in pixels, 
-- based on the 500px image size shown on individual photo pages. 
edit :: NoteID -> Point -> Size -> String -> FM ()
edit nid xy wh txt = withWritePerm $ postMethod $ 
  flickCall_ "flickr.photos.notes.edit"
             [ ("note_id", nid)
	     , ("note_x",   show $ pointX xy)
	     , ("note_y",   show $ pointY xy)
	     , ("note_w",   show $ sizeW  wh)
	     , ("note_h",   show $ sizeH  wh)
	     , ("note_text", txt)
	     ]
