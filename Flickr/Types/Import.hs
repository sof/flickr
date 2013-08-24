--------------------------------------------------------------------
-- |
-- Module      : Flickr.Types.Import
-- Description : Parsing Flickr API responses.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer  : Sigbjorn Finne <sof@forkIO.com>
-- Stability   : provisional
-- Portability : portable
--
-- Translating XML responses into Haskell type representations
-- of the Flickr API resources/entities/types.
--------------------------------------------------------------------
module Flickr.Types.Import where

import Flickr.Types
import Flickr.Utils
import Flickr.Monad ( parseDoc, ErrM )

import Control.Monad ( guard, mplus )
import Data.Char ( toLower )
import Data.Maybe ( mapMaybe )

import Text.XML.Light.Types
import Text.XML.Light.Proc   ( strContent, findChild )

toAuthFrob :: String -> ErrM AuthFrob
toAuthFrob s = parseDoc eltAuthFrob s

eltAuthFrob :: Element -> Maybe AuthFrob
eltAuthFrob e = do
  guard (elName e == nsName "frob")
  return AuthFrob{aFrob = strContent e}

toAuthToken :: String -> ErrM AuthToken
toAuthToken s = parseDoc eltAuthToken s

eltAuthToken :: Element -> Maybe AuthToken
eltAuthToken e = ifNamed "auth" e $ do
  let es = children e
  t    <- pLeaf "token" es
  p    <- pLeaf "perms" es
  user <- pNode "user" es >>= eltUser
  return AuthToken{ authToken = t
                  , authPerms = words p
		  , authUser = user
		  }
		  
toUser :: String -> ErrM User
toUser s = parseDoc eltUser s

eltUser :: Element -> Maybe User
eltUser u = do
  nid   <- pAttr "nsid" u `mplus` pAttr "id" u 
  uname <- pAttr "username" u `mplus` (fmap strContent $ findChild (nsName "username") u)
  let fname = pAttr "fullname" u `mplus` (fmap strContent $ findChild (nsName "realname") u )
  let pro   = eltBool "ispro" u
  let adm   = eltBool "isadmin" u
  return 
     nullUser{ userName     = uname
             , userId       = nid
	     , userFullName = fname
	     , userIsPro    = pro
	     , userIsAdmin  = adm
	     }

toGroupList :: String -> ErrM [Group]
toGroupList s = parseDoc eltGroupList s

eltGroupList :: Element -> Maybe [Group]
eltGroupList e = ifNamed "groups" e $ do
  let ls = pNodes "group" (children e)
  mapM eltGroup ls

toGroup :: String -> ErrM Group
toGroup s = parseDoc eltGroup s

eltGroup :: Element -> Maybe Group
eltGroup u = ifNamed "group" u $ do
  nid   <- pAttr "nsid" u `mplus` pAttr "id" u 
  gname <- pAttr "groupname" u `mplus` (fmap strContent $ findChild (nsName "groupname") u)
  return 
     Group{ groupId      = nid
          , groupName    = gname
	  , groupMembers  = fmap fromIntegral $ eltIntAttr "members" u
	  , groupIsOnline = fmap fromIntegral $ eltIntAttr "members" u
	  , groupChatId   = pAttr "chatid" u `mplus` pAttr "chatnsid" u 
	  , groupInChat   = fmap fromIntegral $ eltIntAttr "inchat" u
	  }

toPlaces :: String -> ErrM (PlaceQuery,[Place])
toPlaces s = parseDoc eltPlaces s

toPlacesList :: String -> ErrM [Place]
toPlacesList s = parseDoc eltPlacesList s

eltPlaceQuery :: Element -> Maybe PlaceQuery
eltPlaceQuery e = ifNamed "places" e $ do
   let qu = pAttr "query" e
   let la = pAttr "latitude" e
   let lo = pAttr "longitude" e
   let ac = pAttr "accuracy" e >>= readMb
   t <- pAttr "total" e >>= readMb
   return 
    PlaceQuery
      { placeQuery          = qu
      , placeQueryLatitude  = la
      , placeQueryLongitude = lo
      , placeQueryAccuracy  = ac
      , placeTotal = t
      }
   
eltPlaces :: Element -> Maybe (PlaceQuery, [Place])
eltPlaces e = ifNamed "places" e $ do
  q  <- eltPlaceQuery e
  let ls = pNodes "place" (children e)
  ps <- mapM eltPlace ls
  return (q, ps)

eltPlacesList :: Element -> Maybe [Place]
eltPlacesList e = ifNamed "places" e $ do
  let ls = pNodes "place" (children e)
  mapM eltPlace ls

eltPlace :: Element -> Maybe Place
eltPlace e = ifNamed "place" e $ do
  pid   <- pAttr "place_id" e
  woeid <- pAttr "woeid" e
  lat   <- pAttr "latitude" e
  long  <- pAttr "longitude" e
  url   <- pAttr "place_url" e
  ty    <- pAttr "place_type" e
  let d = strContent e
  return Place 
    { placeId = pid
    , placeWOEId = woeid
    , placeLat = lat
    , placeLong = long
    , placeURL  = url
    , placeType = ty
    , placeDesc = d
    }
  
toBlogs :: String -> ErrM [Blog]
toBlogs s = parseDoc eltBlogsList s

eltBlogsList :: Element -> Maybe [Blog]
eltBlogsList e = ifNamed "blogs" e $ do
  let ls = pNodes "blog" (children e)
  mapM eltBlog ls

eltBlog :: Element -> Maybe Blog
eltBlog e = ifNamed "blog" e $ do
  bid   <- pAttr "id" e
  nm    <- pAttr "name" e
  npwd  <- eltBool "needspassword" e
  url   <- pAttr "url" e
  return Blog
    { blogId      = bid
    , blogName    = nm
    , blogNeedsPW = npwd
    , blogURL     = url
    }

toPlaceTypes :: String -> ErrM [PlaceType]
toPlaceTypes s = parseDoc eltPlaceTypeList s

eltPlaceTypeList :: Element -> Maybe [PlaceType]
eltPlaceTypeList e = ifNamed "place_types" e $ do
  let ls = pNodes "place_type" (children e)
  mapM eltPlaceType ls

eltPlaceType :: Element -> Maybe PlaceType
eltPlaceType e = ifNamed "place_type" e $ do
  bid   <- pAttr "id" e
  nm    <- pAttr "name" e
  npwd  <- eltBool "needspassword" e
  url   <- pAttr "url" e
  return PlaceType
    { placeTypeId   = fromMaybe "" (pAttr "place_type_id" e)
    , placeTypeName = strContent e
    }

toLocationPlace :: String -> ErrM LocationPlace
toLocationPlace s = parseDoc eltLocationPlace s

eltLocationPlace :: Element -> Maybe LocationPlace
eltLocationPlace e = do
  pid   <- pAttr "place_id" e
  woeid <- pAttr "woeid" e
  lat   <- pAttr "latitude" e
  long  <- pAttr "longitude" e
  url   <- pAttr "place_url" e
  let ty  = fromMaybe (qName $ elName e) $ pAttr "place_type" e
  let d = strContent e
  cs    <- mapM eltLocationPlace (children e)
  return LocationPlace
    { locationPlaceId = pid
    , locationPlaceWOEId = woeid
    , locationPlaceLat = lat
    , locationPlaceLong = long
    , locationPlaceURL  = url
    , locationPlaceType = ty
    , locationPlaceDesc = d
    , locationPlaceDetails = cs
    }

toContentType :: String -> ErrM ContentType
toContentType s = parseDoc eltContentType s

eltContentType :: Element -> Maybe ContentType
eltContentType e = do
  x <- pAttr "content_type" e
  let getV ((v,_):_) = Just (v::Int)
      getV _ = Nothing
  case getV $ reads x of
    Just 1 -> return ContentPhoto
    Just 2 -> return ContentScreenshot
    _      -> return ContentOther

toPrivacy :: String -> String -> ErrM Privacy
toPrivacy x s = parseDoc (eltPrivacy x) s

eltPrivacy :: String -> Element -> Maybe Privacy
eltPrivacy tg e = do
  x <- pAttr tg e
  let getV ((v,_):_) = Just (v::Int)
      getV _ = Nothing
  case getV $ reads x of
    Just 0 -> return Public
    Just 1 -> return Public
    Just 2 -> return (Private False False)
    Just 3 -> return (Private True{-friends-} True{-family-})
    Just 4 -> return (Private True{-friends-} False{-family-})
    Just 5 -> return (Private False{-friends-} True{-family-})
    Just 6 -> return (Private False{-friends-} True{-family-})
    _ -> fail ("unexpected privacy setting: " ++ x)

toBool :: String -> String -> ErrM Bool
toBool x s = parseDoc (eltBool x) s

eltBool :: String -> Element -> Maybe Bool
eltBool tg e = do
  x <- pAttr tg e
  let getV ((v,_):_) = Just (v::Int)
      getV _ = Nothing
  case getV $ reads x of
    Just 0 -> return False
    Just 1 -> return True
    _ -> case map toLower x of
          "true"  -> return True
	  "false" -> return False
	  _ -> fail ("unexpected bool value: " ++ x)

toSafetyLevel :: String -> String -> ErrM Int
toSafetyLevel x s = parseDoc (eltIntAttr x) s

eltIntAttr :: String -> Element -> Maybe Int
eltIntAttr tg e = do
  x <- pAttr tg e
  let getV ((v,_):_) = Just (v::Int)
      getV _ = Nothing
  case getV $ reads x of
    Just v -> return v
    _      -> fail ("unexpected non-Int value: " ++ x)

toString :: String -> String -> ErrM String
toString x s = parseDoc (eltStringAttr x) s

eltStringAttr :: String -> Element -> Maybe String
eltStringAttr tg e = pAttr tg e

toItems :: String -> ErrM [Item]
toItems s = parseDoc eltItems s

eltItems :: Element -> Maybe [Item]
eltItems e = ifNamed "items" e $ do
  let ls = pNodes "item" (children e)
  mapM eltItem ls
  
eltItem :: Element -> Maybe Item
eltItem e = ifNamed "item" e $ do
  ty     <- pAttr "type" e
  iid    <- pAttr "id" e
  own    <- pAttr "owner" e
  prim   <- eltIntAttr "primary" e
  serv   <- pAttr "server" e
  sec    <- pAttr "secret" e
  let comold = fromMaybe 0 $ eltIntAttr "commentsold" e
      comnew = fromMaybe 0 $ eltIntAttr "commentsnew" e
      
      com     = fromMaybe 0 $ eltIntAttr "comments" e
  vie    <- eltIntAttr "views" e
  npho   <- eltIntAttr "photos" e
  more   <- eltBool "more" e
  let tit = fmap strContent $ findChild (nsName "title") e
  let act = findChild (nsName "activity") e >>= eltActivity
  return Item
     { itType     = ty
     , itId       = iid
     , itTitle    = tit
     , itActivity = act
     , itOwner    = own
     , itSecret   = sec
     , itServer   = serv
     , itPhotos   = fromIntegral npho
     , itPrimary  = fromIntegral prim
     , itComments = fromIntegral (com + comold + comnew)
     , itViews    = fromIntegral vie
     , itMore     = more
     }
    
eltActivity :: Element -> Maybe [Activity]
eltActivity e = do
  let es = pNodes "event" (children e)
  mapM eltEvent es

eltEvent :: Element -> Maybe Activity
eltEvent e = do
  ty  <- pAttr "type" e
  uid <- pAttr "user" e
  usr <- pAttr "username" e
  dat <- pAttr "dateadded" e
  let s = strContent e
  return Activity
    { actType = ty
    , actUser = nullUser{userName=usr,userId=uid}
    , actDate = dat
    , actContent = s
    }

toContactList :: String -> ErrM [Contact]
toContactList s = parseDoc eltContactList s

eltContactList :: Element -> Maybe [Contact]
eltContactList e = ifNamed "contacts" e $ do
  let ls = pNodes "contact" (children e)
  mapM eltContact ls

eltContact :: Element -> Maybe Contact
eltContact e = do
  cid <- pAttr "nsid" e
  usr <- eltUser e
  let ico = eltBool "iconserver" e
  let fri = eltBool "friend" e
  let fam = eltBool "family" e
  let ign = eltBool "ignored" e
  return Contact
    { conId = cid
    , conUser = usr
    , conIcon = ico
    , conIsFriend = fri
    , conIsFamily = fam
    , conIgnored  = ign
    }
     
  
toPhotoList :: String -> ErrM (PhotoContext, [Photo])
toPhotoList s = parseDoc eltPhotoList s

toPhotoPair :: String -> ErrM (Photo,Photo)
toPhotoPair s = parseDoc eltPhotoPair s

eltPhotoList :: Element -> Maybe (PhotoContext, [Photo])
eltPhotoList e = ifNamed "photos" e $ do
  ls <- mapM eltPhoto $ pNodes "photo" (children e)
  c  <- eltPhotoContext e
  return (c, ls)

eltPhotoPair :: Element -> Maybe (Photo, Photo)
eltPhotoPair e = do
  f <- findChild (nsName "prevphoto") e >>= eltPhoto 
  s <- findChild (nsName "nextphoto") e >>= eltPhoto 
  return (f,s)

eltPhoto :: Element -> Maybe Photo
eltPhoto e = do
  pid <- pAttr "id" e
  let own = pAttr "owner" e
  sec <- pAttr "secret" e
  tit <- pAttr "title" e `mplus` fmap strContent (findChild (nsName "title") e)
  let url = pAttr "url" e
  return Photo
     { photoId      = pid
     , photoOwner   = fmap (\ x -> nullUser{userId=x}) own
     , photoURL     = url
     , photoSecret  = sec
     , photoServer  = fmap fromIntegral (eltIntAttr "server" e)
     , photoFarm    = pAttr "farm" e
     , photoLicense = pAttr "license" e
     , photoTitle   = tit
     , photoPublic  = eltBool "ispublic" e
     , photoFriend  = eltBool "isfriend" e
     , photoFamily  = eltBool "isfamily" e
     }

eltPhotoContext :: Element -> Maybe PhotoContext
eltPhotoContext e = 
 return PhotoContext
     { photoCtxtPage    = eltIntAttr "page" e
     , photoCtxtPages   = eltIntAttr "pages" e
     , photoCtxtPerPage = eltIntAttr "perpage" e
     , photoCtxtTotal   = eltIntAttr "total" e
     }

toCategory :: String -> ErrM Category
toCategory s = parseDoc eltCategory s

eltCategory :: Element -> Maybe Category
eltCategory e = do
  nm  <- pAttr "name" e
  pth <- pAttr "path" e
  let mid = pAttr "id" e
  pts <- pAttr "pathids" e
  let ls = children e
  let cs  = mapMaybe eltGroupCat ls
  return Category
   { catName = nm
   , catId   = mid
   , catPath = pth
   , catPaths = pts
   , catSubs  = cs
   }
  
eltGroupCat :: Element -> Maybe GroupCat
eltGroupCat e 
 | elName e == nsName "subcat" = eltSubCategory e >>= \ x -> return (SubCat x)
 | elName e == nsName "group"  = eltGroup e  >>= \ x -> return (AGroup x)
 | otherwise = Nothing

eltSubCategory :: Element -> Maybe SubCategory
eltSubCategory e = do
  cid <- pAttr "id" e
  nm  <- pAttr "name" e
  c   <- eltIntAttr "count" e
  return SubCategory
    { subCatId = cid
    , subName  = nm
    , subCount = fromIntegral c
    }

eltBandwidth :: Element -> Maybe Bandwidth
eltBandwidth e = do
  mx <- eltIntAttr "maxbytes" e
  let xkb = eltIntAttr "maxkb" e
  us <- eltIntAttr "usedbytes" e
  let uskb = eltIntAttr "usedkb" e
  re <- eltIntAttr "remainingbytes" e
  let rekb = eltIntAttr "remainingkb" e
  return Bandwidth
    { bandWidthBytes          = fromIntegral mx
    , bandWidthKB             = fmap fromIntegral xkb
    , bandWidthUsedBytes      = fromIntegral us
    , bandWidthUsedKB         = fmap fromIntegral uskb
    , bandWidthRemainingBytes = fromIntegral re
    , bandWidthRemainingKB    = fmap fromIntegral rekb
    }

eltFileSize :: Element -> Maybe FileSize
eltFileSize e = do
  fs <- eltIntAttr "maxbytes" e
  let fskb = eltIntAttr "maxkb" e
  return FileSize
    { fileSizeBytes = fromIntegral fs
    , fileSizeKB    = fmap fromIntegral fskb
    }

eltPhotosetQuota :: Element -> Maybe PhotosetQuota
eltPhotosetQuota e = do
  c <- eltIntAttr "created" e
  z <- pAttr "remaining" e
  let 
   f = case z of
        "remaining" -> Nothing
	x -> case reads x of
	      ((v,_):_) -> Just v
	      _ -> Nothing
  return PhotosetQuota
    { photosetCreated = fromIntegral c
    , photosetRemaining = f
    }

toPhotoset :: String -> ErrM Photoset
toPhotoset s = parseDoc eltPhotoset s

eltPhotoset :: Element -> Maybe Photoset
eltPhotoset e = do
  pid <- pAttr "id" e
  uid <- pAttr "owner" e
  prim <- pAttr "primary" e
  c    <- eltIntAttr "photos" e
  tit  <- pAttr "title" e
  desc <- pAttr "description" e
  return Photoset
     { photosetId           = pid
     , photosetOwner        = uid
     , photosetPrimaryPhoto = prim
     , photosetPhotos       = c
     , photosetTitle        = tit
     , photosetDescription  = desc
     }

toPhotoPool :: String -> ErrM PhotoPool
toPhotoPool s = parseDoc eltPhotoPool s

eltPhotoPool :: Element -> Maybe PhotoPool
eltPhotoPool e = do
  pid <- pAttr "id" e
  tit <- pAttr "title" e
  return PhotoPool
    { photoPoolId = pid
    , photoPoolTitle = tit
    }

toPhotoDetails :: String -> ErrM PhotoDetails
toPhotoDetails s = parseDoc eltPhotoDetails s

eltPhotoDetails :: Element -> Maybe PhotoDetails
eltPhotoDetails e = do
  ph  <- eltPhoto e
  let
   rot = eltIntAttr "rotation" e
   fav = eltBool "isfavorite" e
   lic = pAttr "license" e
   ofm = pAttr "originalformat" e
   ose = pAttr "originalsecret" e
   tit = fmap strContent (findChild (nsName "title") e)
   des = fmap strContent (findChild (nsName "description") e)

   es = children e

   isp = pNode "visibility" es >>= eltBool "ispublic"
   fam = pNode "visibility" es >>= eltBool "isfamily"
   fri = pNode "visibility" es >>= eltBool "isfriend"

   per = do
     ch <- pNode "permissions" es
     a  <- eltIntAttr "permcomment" ch
     b  <- eltIntAttr "permaddmeta" ch
     return (a,b)

   edi = do
     ch <- pNode "editability" es
     a  <- eltBool "cancomment" ch
     b  <- eltBool "canaddmeta" ch
     return (a,b)
   ns = mapMaybe eltNote (fromMaybe [] $ fmap children $ pNode "notes" es)
   ts = mapMaybe eltTagDetails (fromMaybe [] $ fmap children $ pNode "tags" es)
   us = mapMaybe eltURLDetails (fromMaybe [] $ fmap children $ pNode "urls" es)

  d   <- pNode "dates" es >>= eltPhotoDate     
  return PhotoDetails
     { photoDetailsPhoto       = ph
     , photoDetailsRotation    = rot
     , photoDetailsLicense     = lic
     , photoDetailsIsFavorite  = fav
     , photoDetailsIsPublic    = isp
     , photoDetailsIsFamily    = fam
     , photoDetailsIsFriend    = fri
     , photoDetailsOrigFormat  = ofm
     , photoDetailsOrigSecret  = ose
     , photoDetailsTitle       = tit
     , photoDetailsDesc        = des
     , photoDetailsDates       = d
     , photoDetailsPerms       = per
     , photoDetailsEdits       = edi
     , photoDetailsComments    = pNode "comments" es >>= intContent
     , photoDetailsNotes       = ns
     , photoDetailsTags        = ts
     , photoDetailsURLs        = us
     }

eltPhotoDate :: Element -> Maybe PhotoDate
eltPhotoDate e = do
   p <- pAttr "posted" e
   t <- pAttr "taken" e
   l <- pAttr "lastupdate" e
   return PhotoDate
     { photoDatePosted      = p
     , photoDateTaken       = t
     , photoDateLastUpdate  = l
     , photoDateGranularity = eltIntAttr "takengranularity" e
     }

eltNote :: Element -> Maybe Note
eltNote e = do
  i   <- pAttr "id" e
  uid <- pAttr "author" e
  nm  <- pAttr "authorname" e
  let x = eltIntAttr "x" e
      y = eltIntAttr "y" e
      w = eltIntAttr "w" e
      h = eltIntAttr "h" e
      s = strContent e
  return Note
    { noteId         = i
    , noteAuthor     = uid
    , noteAuthorName = nm
    , notePoint      = x >>= \ xv -> y >>= \ yv -> return (Point xv yv)
    , noteSize       = w >>= \ wv -> h >>= \ hv -> return (Size wv hv)
    , noteText       = s
    }

eltTagDetails :: Element -> Maybe TagDetails
eltTagDetails e = do
  i   <- pAttr "id" e
  uid <- pAttr "author" e
  let c  = eltIntAttr "count" e
  let s  = eltIntAttr "score" e
  rs <- (pAttr "raw" e >>= \ x -> return [x]) `mplus`
        (return (map strContent (pNodes "raw" (children e))))
  return TagDetails
    { tagDetailsId     = i
    , tagDetailsAuthor = uid
    , tagDetailsRaw    = rs
    , tagDetailsName   = strContent e
    , tagDetailsCount  = c
    , tagDetailsScore  = s
    }

eltURLDetails :: Element -> Maybe URLDetails
eltURLDetails e = do
  ty <- pAttr "type" e
  return URLDetails
    { urlDetailsType = ty
    , urlDetailsURL  = strContent e
    }

toPhotoCountList :: String -> ErrM [PhotoCount]
toPhotoCountList s = parseDoc eltPhotoCountList s

eltPhotoCountList :: Element -> Maybe [PhotoCount]
eltPhotoCountList e = ifNamed "photocounts" e $ do
  let ls = mapMaybe eltPhotoCount $ pNodes "photocount" (children e)
  return ls

eltPhotoCount :: Element -> Maybe PhotoCount
eltPhotoCount e = ifNamed "photocount" e $ do
  c <- eltIntAttr "count" e
  fd <- pAttr "fromdate" e
  td <- pAttr "todate" e
  return PhotoCount
    { photoCount = c
    , photoCountFrom = fd
    , photoCountTo   = td
    }

toEXIFList :: String -> ErrM [EXIF]
toEXIFList s = parseDoc eltEXIFList s

eltEXIFList :: Element -> Maybe [EXIF]
eltEXIFList e = do
  let ls = mapMaybe eltEXIF $ pNodes "exif" (children e)
  return ls

eltEXIF :: Element -> Maybe EXIF
eltEXIF e = ifNamed "exif" e $ do
  ts   <- pAttr "tagspace" e
  tsid <- pAttr "tagspaceid" e
  tid  <- pAttr "tag" e
  lbl  <- pAttr "label" e
  let rw = fmap strContent $ findChild (nsName "raw") e
  let cl = fmap strContent $ findChild (nsName "clean") e
  return EXIF
    { exifTag = EXIFTag{exifTagId=tid,exifTagspace=ts,exifTagspaceId=tsid}
    , exifLabel = lbl
    , exifRaw   = rw
    , exifClean = cl
    }

toPermissions :: String -> ErrM Permissions
toPermissions s = parseDoc eltPermissions s

eltPermissions :: Element -> Maybe Permissions
eltPermissions e = do
  i  <- pAttr "id" e
  pu <- eltBool "ispublic" e
  fa <- eltBool "isfamily" e
  fr <- eltBool "isfriend" e
  pc <- eltIntAttr "permcomment" e
  pa <- eltIntAttr "permaddmeta" e
  return Permissions
      { permId         = i
      , permIsPublic   = pu
      , permIsFriend   = fr
      , permIsFamily   = fa
      , permCommentLevel = pc
      , permAddMetaLevel = pa
      }

toSizeList :: String -> ErrM [SizeDetails]
toSizeList s = parseDoc eltSizeList s

eltSizeList :: Element -> Maybe [SizeDetails]
eltSizeList e = do
  let ls = mapMaybe eltSize $ pNodes "size" (children e)
  return ls

eltSize :: Element -> Maybe SizeDetails
eltSize e = ifNamed "size" e $ do
  la   <- pAttr "label" e
  w   <- eltIntAttr "width" e
  h   <- eltIntAttr "height" e
  src <- pAttr "source" e
  url <- pAttr "url" e
  return SizeDetails
     { sizeDetailsLabel  = la
     , sizeDetailsWidth  = w
     , sizeDetailsHeight = h
     , sizeDetailsSource = src
     , sizeDetailsURL    = url
     }

toPhotoID :: String -> ErrM PhotoID
toPhotoID s = parseDoc eltPhotoID s

eltPhotoID :: Element -> Maybe PhotoID
eltPhotoID e = ifNamed "photoid" e $ return (strContent e)

toCommentID :: String -> ErrM CommentID
toCommentID s = parseDoc eltCommentID s

eltCommentID :: Element -> Maybe CommentID
eltCommentID e = pAttr "id" e -- that wasn't too hard, was it?

toNoteID :: String -> ErrM NoteID
toNoteID s = parseDoc eltNoteID s

eltNoteID :: Element -> Maybe NoteID
eltNoteID e = pAttr "id" e

toCommentList :: String -> ErrM [Comment]
toCommentList s = parseDoc eltCommentList s

eltCommentList :: Element -> Maybe [Comment]
eltCommentList e = 
  return $ mapMaybe eltComment $ pNodes "comment" (children e)

eltComment :: Element -> Maybe Comment
eltComment e = ifNamed "comment" e $ do
   i   <- pAttr "id" e
   au  <- eltUser e
   da  <- pAttr "datecreate" e
   return Comment
     { commentId     = i
     , commentAuthor = au
     , commentDate   = da
     , commentURL    = pAttr "permalink" e `mplus` pAttr "url" e
     , commentText   = strContent e
     }

toGeoLocation :: String -> ErrM GeoLocation
toGeoLocation s = parseDoc eltGeoLocation s

eltGeoLocation :: Element -> Maybe GeoLocation
eltGeoLocation e = ifNamed "location" e $ do
   la  <- pAttr "latitude" e
   lo  <- pAttr "longitude" e
   let ac = eltIntAttr "accuracy" e
   return (la,lo,ac)

toLicenseList :: String -> ErrM [License]
toLicenseList s = parseDoc eltLicenseList s

eltLicenseList :: Element -> Maybe [License]
eltLicenseList e = 
  return $ mapMaybe eltLicense $ pNodes "license" (children e)

eltLicense :: Element -> Maybe License
eltLicense e = ifNamed "license" e $ do
  i   <- pAttr "id" e
  nm  <- pAttr "name" e
  url <- pAttr "url" e
  return License
   { licenseId   = i
   , licenseName = nm
   , licenseLink = url
   }

toTicketList :: String -> ErrM [Ticket]
toTicketList s = parseDoc eltTicketList s

eltTicketList :: Element -> Maybe [Ticket]
eltTicketList e = 
  return $ mapMaybe eltTicket $ pNodes "ticket" (children e)

eltTicket :: Element -> Maybe Ticket
eltTicket e = ifNamed "ticket" e $ do
   i   <- pAttr "id" e
   c   <- eltIntAttr "complete" e
   p   <- pAttr "photoid" e
   let isInv = fromMaybe False (eltBool "invalid" e)
   return Ticket
      { ticketId       = i
      , ticketComplete = c
      , ticketInvalid  = isInv
      , ticketPhoto    = p
      }

toClusterList :: String -> ErrM [Cluster]
toClusterList s = parseDoc eltClusterList s

eltClusterList :: Element -> Maybe [Cluster]
eltClusterList e = 
  return $ mapMaybe eltCluster $ pNodes "cluster" (children e)

eltCluster :: Element -> Maybe Cluster
eltCluster e = ifNamed "cluster" e $ do
   t   <- eltIntAttr "total" e
   let ts = pNodes "tag" (children e)
   return Cluster
      { clusterCount   = t
      , clusterTags    = map strContent ts
      }

toTagDetailsList :: String -> ErrM [TagDetails]
toTagDetailsList s = parseDoc eltTagDetailsList s

eltTagDetailsList :: Element -> Maybe [TagDetails]
eltTagDetailsList e = do
  t <- pNode "tags" (children e)
  return $ mapMaybe eltTagDetails $ pNodes "tag" (children t)

toNamespaceList :: String -> ErrM (NameContext, [Namespace])
toNamespaceList s = parseDoc eltNamespaceList s

eltNamespaceList :: Element -> Maybe (NameContext, [Namespace])
eltNamespaceList e = ifNamed "namespaces" e $ do
  ls <- mapM eltNamespace $ pNodes "namespace" (children e)
  c  <- eltResContext e
  return (c, ls)

eltResContext :: Element -> Maybe (ResContext a)
eltResContext e = 
 return ResContext
     { resCtxtPage    = eltIntAttr "page" e
     , resCtxtPages   = eltIntAttr "pages" e
     , resCtxtPerPage = eltIntAttr "perpage" e
     , resCtxtTotal   = eltIntAttr "total" e
     }

eltNamespace :: Element -> Maybe Namespace
eltNamespace e = ifNamed "namespace" e $ do
  return Namespace
    { namespaceUsage = fromIntegral $ fromMaybe 0 (eltIntAttr "usage" e)
    , namespacePreds = fromIntegral $ fromMaybe 0 (eltIntAttr "predicates" e)
    , namespaceName  = strContent e
    }

eltMTPair :: Element -> Maybe MachineTagPair
eltMTPair e = ifNamed "pair" e $ do
  return MachineTagPair
    { mtPairNamespace = fromMaybe "" (pAttr "namespace" e)
    , mtPairPredicate = fromMaybe "" (pAttr "predicate" e)
    , mtPairUsage     = fromIntegral $ fromMaybe 0 (eltIntAttr "usage" e)
    , mtPairName      = strContent e
    }

eltMTPred :: Element -> Maybe MachineTagPred
eltMTPred e = ifNamed "predicate" e $ do
  return MachineTagPred
    { mtPredNamespaces = fromIntegral $ fromMaybe 0 (eltIntAttr "namespaces" e)
    , mtPredUsage      = fromIntegral $ fromMaybe 0 (eltIntAttr "usage" e)
    , mtPredName       = strContent e
    }

eltMTag :: Element -> Maybe MachineTag
eltMTag e = ifNamed "value" e $ do
  return MachineTag
    { mTagNamespace = ""
    , mTagPredicate = ""
    , mTagUsage     = fromIntegral $ fromMaybe 0 (eltIntAttr "usage" e)
    , mTagValue     = strContent e
    }

toMachineTagList :: String -> ErrM (ResContext MachineTag, [MachineTag])
toMachineTagList s = parseDoc eltMachineTagList s

eltMachineTagList :: Element -> Maybe (ResContext MachineTag, [MachineTag])
eltMachineTagList e = ifNamed "values" e $ do
  ls <- mapM eltMTag $ pNodes "value" (children e)
  c  <- eltResContext e
  return (c, ls)

toPredList :: String -> ErrM (ResContext MachineTagPred, [MachineTagPred])
toPredList s = parseDoc eltMachinePredList s

eltMachinePredList :: Element -> Maybe (ResContext MachineTagPred, [MachineTagPred])
eltMachinePredList e = ifNamed "predicates" e $ do
  ls <- mapM eltMTPred $ pNodes "predicate" (children e)
  c  <- eltResContext e
  return (c, ls)

toPairList :: String -> ErrM (ResContext MachineTagPair, [MachineTagPair])
toPairList s = parseDoc eltMachinePairList s

eltMachinePairList :: Element -> Maybe (ResContext MachineTagPair, [MachineTagPair])
eltMachinePairList e = ifNamed "pairs" e $ do
  ls <- mapM eltMTPair $ pNodes "pair" (children e)
  c  <- eltResContext e
  return (c, ls)

toTagInfoList :: String -> ErrM [TagInfo]
toTagInfoList s = parseDoc eltTagInfoList s

eltTagInfoList :: Element -> Maybe [TagInfo]
eltTagInfoList e = ifNamed "tags" e $ do
  mapM eltTagInfo $ pNodes "tag" (children e)

eltTagInfo :: Element -> Maybe TagInfo
eltTagInfo e = do
  let c = pAttr "count" e
  return TagInfo
    { tagName  = strContent e
    , tagCount = maybe Nothing id (fmap readMb c)
    }
