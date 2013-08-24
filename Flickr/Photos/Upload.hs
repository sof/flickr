--------------------------------------------------------------------
-- |
-- Module      : Flickr.Photos.Upload
-- Description : flickr.photos.upload - check upload status.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer  : Sigbjorn Finne <sof@forkIO.com>
-- Stability   : provisional
-- Portability : portable
--
-- flickr.photos.upload API, check status of upload API initiated actions.
--------------------------------------------------------------------
module Flickr.Photos.Upload where

import Flickr.Monad
import Flickr.Types
import Flickr.Types.Import
import Flickr.Utils

import Text.XML.Light.Proc   ( strContent )

import Data.List

-- | Checks the status of one or more asynchronous photo upload tickets.
checkTickets :: [TicketID] -> FM [Ticket]
checkTickets tids = 
  flickTranslate toTicketList $
    flickrCall "flickr.photos.upload.checkTickets"
               [ ("tickets", intercalate "," tids) ]

-- | Upload a photo, returning an upload id/ticket.
uploadPhoto :: FilePath     -- ^ local file to upload
            -> Maybe String -- ^ title
            -> Maybe String -- ^ description
	    -> [Tag]
	    -> UploadAttr
	    -> FM PhotoID
uploadPhoto photo title desc tgs attr = withWritePerm $ withBase upload_base $ postMethod $ 
  flickTranslate toPhotoID $ 
   flickCall ""
             (mbArg "title" title $
	      mbArg "description" desc $
	      lsArg "tags"  tgs $
	      mbArg "is_public" (fmap showBool $ uploadPublic attr) $ 
	      mbArg "is_friend" (fmap showBool $ uploadFriend attr) $ 
	      mbArg "is_family" (fmap showBool $ uploadFamily attr) $ 
	      mbArg "safety_level"   (fmap showSafety $ uploadSafety attr) $ 
	      mbArg "content_type"    (fmap showContentType $ uploadContentType attr) $ 
	      mbArg "hidden"    (fmap showBool $ uploadHidden attr) [("photo",'@':photo)])

data UploadAttr
 = UploadAttr
     { uploadPublic :: Maybe Bool
     , uploadFriend :: Maybe Bool
     , uploadFamily :: Maybe Bool
     , uploadSafety :: Maybe Safety
     , uploadContentType :: Maybe ContentType
     , uploadHidden :: Maybe Bool
     }

nullUploadAttr :: UploadAttr
nullUploadAttr = UploadAttr
     { uploadPublic = Nothing
     , uploadFriend = Nothing
     , uploadFamily = Nothing
     , uploadSafety = Nothing
     , uploadContentType = Nothing
     , uploadHidden = Nothing
     }

-- | Upload a photo, returning an upload id/ticket.
replacePhoto :: FilePath     -- ^ local file to upload/replace.
             -> PhotoID
	     -> Maybe Bool
	     -> FM (String,String, PhotoID)
replacePhoto photo pid mbAsync = withWritePerm $ withBase replace_base $ postMethod $ 
  flickTranslate toRes $ 
   flickCall ""
             (mbArg "async" (fmap showBool mbAsync) $ 
	            [ ("photo_id", pid)
		    , ("photo",'@':photo)
		    ])
 where 
  toRes s = parseDoc eltRes s
  
  eltRes e = do
    s  <- pAttr "secret" e
    os <- pAttr "originalsecret" e
    return (s,os,strContent e)



