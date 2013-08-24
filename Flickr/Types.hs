--------------------------------------------------------------------
-- |
-- Module      : Flickr.Types
-- Description : Collection of Haskell types for Flickr
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer  : Sigbjorn Finne <sof@forkIO.com>
-- Stability   : provisional
-- Portability : portable
--
-- Haskell rendering of types used and introduced by the Flickr API.
--------------------------------------------------------------------
module Flickr.Types where

--import Flickr.Monad
import Text.XML.Light.Types as XML


-- these should come from somewhere else..
type URLString = String
type DateString = String
type UserID     = String
type UserName   = String
type NSID       = UserID
type PhotoID    = String
type PhotosetID = String
type BlogID     = String
type CategoryID = String
type ChatId     = String
type Key        = String
type Tag        = String
type TagID      = Tag
type Title      = String
type Description = String
type PlaceID    = String
type GroupID    = String
type WhereOnEarthID = String
type LicenseID = String
type CommentID = String
type TicketID  = String
type NoteID    = String
type ClusterID = String
type DayWeek   = Bool -- False => Day; True => Week
type PermissionID = String
type ContextID = Integer

type Date = DateString

type DateInterval = (Date,Maybe Date)

data AppInfo
 = AppInfo
      { appTitle       :: String
      , appDescription :: String
      , appLogo        :: URLString
      , appAboutURL    :: Maybe URLString
      }

data Signature = Signature { sigComputed :: String }

data AuthRequest
 = AuthRequest
     { authKey        :: Key
     , authPermission :: String
     , authFrob       :: Maybe AuthFrob
     , authSig        :: Signature 
     }
     
data AuthFrob = AuthFrob { aFrob :: String }

type AuthTokenValue = String
type AuthMiniToken  = String

data AuthToken
 = AuthToken
     { authToken :: AuthTokenValue
     , authPerms :: [String] -- many/one ?
     , authUser  :: User
     }

data User
 = User
     { userName       :: UserName
     , userId         :: String
     , userFullName   :: Maybe String  -- aka 'real name'
     , userIsAdmin    :: Maybe Bool
     , userIsPro      :: Maybe Bool
     , userLocation   :: Maybe String
     , userProfileURL :: Maybe URLString
     , userPhotosURL  :: Maybe URLString
     , userPhotoStat  :: Maybe UserPhotoStat
     , userAttrs      :: [XML.Attr]
     }

nullUser :: User
nullUser
 = User
     { userName       = ""
     , userId         = ""
     , userFullName   = Nothing
     , userIsAdmin    = Nothing
     , userIsPro      = Nothing
     , userLocation   = Nothing
     , userProfileURL = Nothing
     , userPhotosURL  = Nothing
     , userPhotoStat  = Nothing
     , userAttrs      = []
     }


data UserPhotoStat
 = UserPhotoStat
     { userPhotoFirst :: Maybe DateString
     , userPhotoCount :: Maybe Int
     }

data Activity
 = Activity
     { actType    :: String
     , actUser    :: User
     , actDate    :: DateString
     , actContent :: String
     } 

data Item
 = Item
     { itType     :: String
     , itId       :: String
     , itTitle    :: Maybe String
     , itActivity :: Maybe [Activity]
     , itOwner    :: UserID
     , itSecret   :: String
     , itServer   :: String
     , itComments :: Integer
     , itViews    :: Integer
     , itPhotos   :: Integer
     , itPrimary  :: Integer
     , itMore     :: Bool
     }
     
data Blog
 = Blog
     { blogId      :: BlogID
     , blogName    :: String
     , blogNeedsPW :: Bool
     , blogURL     :: URLString
     }

data Photo
 = Photo
     { photoId     :: PhotoID
     , photoTitle  :: String
     , photoOwner  :: Maybe User
     , photoSecret :: String
     , photoServer :: Maybe Integer
     , photoFarm   :: Maybe String
     , photoURL    :: Maybe URLString
     , photoLicense :: Maybe LicenseID
     , photoPublic :: Maybe Bool
     , photoFriend :: Maybe Bool
     , photoFamily :: Maybe Bool
     }

data PhotoDetails
 = PhotoDetails
     { photoDetailsPhoto    :: Photo
     , photoDetailsRotation :: Maybe Int
     , photoDetailsLicense  :: Maybe LicenseID
     , photoDetailsIsFavorite :: Maybe Bool
     , photoDetailsIsPublic   :: Maybe Bool
     , photoDetailsIsFamily   :: Maybe Bool
     , photoDetailsIsFriend   :: Maybe Bool
     , photoDetailsOrigFormat :: Maybe String
     , photoDetailsOrigSecret :: Maybe String
     , photoDetailsTitle    :: Maybe String
     , photoDetailsDesc     :: Maybe String
     , photoDetailsDates    :: PhotoDate
     , photoDetailsPerms    :: Maybe (Int,Int)
     , photoDetailsEdits    :: Maybe (Bool,Bool)
     , photoDetailsComments :: Maybe Int
     , photoDetailsNotes    :: [Note]
     , photoDetailsTags     :: [TagDetails]
     , photoDetailsURLs     :: [URLDetails]
     }
     
data PhotoSize
 = PhotoSizeSmallSquare
 | PhotoSizeThumb
 | PhotoSizeSmall
 | PhotoSizeMedium
 | PhotoSizeLarge
 | PhotoSizeOriginal
   deriving ( Eq, Enum )

data PhotoDate
 = PhotoDate
     { photoDatePosted :: DateString
     , photoDateTaken  :: DateString
     , photoDateLastUpdate :: DateString     
     , photoDateGranularity :: Maybe Int
     }

data Note
 = Note 
    { noteId         :: NoteID
    , noteAuthor     :: UserID
    , noteAuthorName :: UserName
    , notePoint      :: Maybe Point
    , noteSize       :: Maybe Size
    , noteText       :: String
    }

data PhotoInfo
 = PhotoLicense
 | PhotoDateUpload
 | PhotoDateTaken
 | PhotoOwnerName
 | PhotoIconServer
 | PhotoOriginalFormat
 | PhotoLastUpdate
 | PhotoGeo
 | PhotoTags
 | PhotoMachineTags
 | PhotoO_Dims
 | PhotoViews
 | PhotoMedia
 
instance Show PhotoInfo where
  show x = 
   case x of
     PhotoLicense -> "license"
     PhotoDateUpload -> "date_upload"
     PhotoDateTaken -> "date_taken"
     PhotoOwnerName -> "owner_name"
     PhotoIconServer -> "icon_server"
     PhotoOriginalFormat -> "original_format"
     PhotoLastUpdate     -> "last_update"
     PhotoGeo -> "geo"
     PhotoTags -> "tags"
     PhotoMachineTags -> "machine_tags"
     PhotoO_Dims -> "o_dims"
     PhotoViews -> "views"
     PhotoMedia -> "media"

data Photoset
 = Photoset 
     { photosetId           :: PhotoID
     , photosetOwner        :: UserID
     , photosetPrimaryPhoto :: PhotoID
     , photosetPhotos       :: Int
     , photosetTitle        :: String
     , photosetDescription  :: String
     }

data PhotoPool
 = PhotoPool
     { photoPoolId    :: PhotoID
     , photoPoolTitle :: String
     }
     
data PhotoContext
 = PhotoContext
     { photoCtxtPage    :: Maybe Int
     , photoCtxtPages   :: Maybe Int
     , photoCtxtPerPage :: Maybe Int
     , photoCtxtTotal   :: Maybe Int
     }


data PhotoCount
 = PhotoCount
     { photoCount     :: Int
     , photoCountFrom :: Date
     , photoCountTo   :: Date
     }

data MediaType
 = Photos
 | Videos
 | All

instance Show MediaType where
  show All = "all"
  show Photos = "photos"
  show Videos = "videos"
 
data Privacy
 = Public
 | Contacts
 | Private Bool{-for friends?-} Bool{-for family?-}

instance Enum Privacy where
  fromEnum x = 
    case x of
      Public -> 1
      Contacts -> 2 -- hmm..
      Private True False -> 2
      Private False True -> 3
      Private True True  -> 4
      Private False False -> 5
      
  toEnum x = 
   case x of
     0 -> Public
     1 -> Public
     2 -> Private True False
     3 -> Private False True
     4 -> Private True True
     5 -> Private False False
     _ -> Contacts


data SortKey
 = SortKey
      { sortKind :: String
      , sortDir  :: AscDesc
      }

data AscDesc = Asc | Desc

instance Show AscDesc where
  show Asc = "asc"
  show Desc = "desc"

instance Show SortKey where
 show x = sortKind x ++ '-':show (sortDir x)

data EXIF
 = EXIF
   { exifTag      :: EXIFTag
   , exifLabel    :: String
   , exifRaw      :: Maybe String
   , exifClean    :: Maybe String
   }
   
data EXIFTag
  = EXIFTag
     { exifTagId      :: Tag
     , exifTagspace   :: String
     , exifTagspaceId :: Tag
     }
     
data Safety 
  = Safe | Moderate | Restricted
    deriving ( Enum )

showSafety :: Safety -> String
showSafety s = show (succ (fromEnum s))

type Decimal = String -- for now.

type DateGranularity = Int
 -- 0 => Y-m-d H:i:s
 -- 4 => Y-m
 -- 6 => Y

data ContentType 
 = ContentPhoto | ContentScreenshot | ContentOther
   deriving ( Enum )
	
showContentType :: ContentType -> String
showContentType c = show (succ (fromEnum c))

type Latitude  = Decimal
type Longitude = Decimal
type GeoLocation = (Latitude, Longitude, Maybe Accuracy)

data LocationPlace
 = LocationPlace
     { locationPlaceId      :: PlaceID
     , locationPlaceWOEId   :: WhereOnEarthID
     , locationPlaceLat     :: Latitude
     , locationPlaceLong    :: Longitude
     , locationPlaceURL     :: URLString
     , locationPlaceType    :: PlaceTypeName
     , locationPlaceDetails :: [LocationPlace]
     , locationPlaceDesc    :: String
     }

type Accuracy = Int -- range: 1-16
 -- 1 => World level
 -- 3 => Country
 -- 6 => Region
 -- 11 => City
 -- 16 => Street

data BoundingBox
 = BoundingBox
    { bboxMinLongitude :: Int
    , bboxMinLatitude  :: Int
    , bboxMaxLongitude :: Int -- [-180...180]
    , bboxMaxLatitude  :: Int -- [-90..90]
    }

instance Show BoundingBox where
 show x = shows (bboxMinLongitude x) 
            (',':shows (bboxMinLatitude x) 
	       (',':shows (bboxMaxLongitude x)
 	        (',':shows (bboxMaxLatitude x) "")))
    
data Size = Size { sizeW :: Int, sizeH :: Int}
data Point = Point { pointX :: Int, pointY :: Int}

data Comment
 = Comment { commentId     :: CommentID
           , commentAuthor :: User
	   , commentDate   :: Date
	   , commentURL    :: Maybe URLString
	   , commentText   :: String
	   }
	   
data Permissions
 = Permissions
      { permId         :: PermissionID
      , permIsPublic   :: Bool
      , permIsFriend   :: Bool
      , permIsFamily   :: Bool
      , permCommentLevel :: Int  -- 0 = nobody, 1 = friends&fam, 2 = contacts, 3 = everybody
      , permAddMetaLevel :: Int -- same
      }
      
data Ticket
 = Ticket
      { ticketId       :: TicketID
      , ticketComplete :: Int
      , ticketInvalid  :: Bool
      , ticketPhoto    :: PhotoID
      }
      
data License
 = License { licenseId :: LicenseID
           , licenseName :: String
	   , licenseLink :: URLString
	   }
	   
 
data PlaceQuery 
 = PlaceQuery 
     { placeQuery          :: Maybe String
     , placeQueryLatitude  :: Maybe Decimal
     , placeQueryLongitude :: Maybe Decimal
     , placeQueryAccuracy  :: Maybe Accuracy
     , placeTotal          :: Int
     }

type Threshold = Int

type PlaceTypeName = String
type PlaceTypeId   = String

data PlaceType
 = PlaceType
     { placeTypeId   :: PlaceTypeId
     , placeTypeName :: PlaceTypeName
     }

data Place
 = Place
     { placeId    :: PlaceID
     , placeWOEId :: WhereOnEarthID
     , placeLat   :: Decimal
     , placeLong  :: Decimal
     , placeURL   :: URLString
     , placeType  :: PlaceTypeName -- accuracy string.
     , placeDesc  :: String
     }


data GroupCat
 = SubCat SubCategory
 | AGroup Group
 
data Category
 = Category
    { catName   :: String
    , catId     :: Maybe CategoryID
    , catPath   :: String
    , catPaths  :: String
    , catSubs   :: [GroupCat]
    }

data Group
 = Group
    { groupId       :: GroupID
    , groupName     :: String
    , groupMembers  :: Maybe Integer
    , groupIsOnline :: Maybe Integer
    , groupChatId   :: Maybe ChatId
    , groupInChat   :: Maybe Integer
    }
    
data SubCategory
 = SubCategory
    { subCatId :: CategoryID
    , subName  :: String
    , subCount :: Integer
    }

data FileSize
 = FileSize
    { fileSizeBytes :: Integer
    , fileSizeKB    :: Maybe Integer
    }

data Bandwidth
 = Bandwidth
    { bandWidthBytes          :: Integer
    , bandWidthKB             :: Maybe Integer
    , bandWidthUsedBytes      :: Integer
    , bandWidthUsedKB         :: Maybe Integer
    , bandWidthRemainingBytes :: Integer
    , bandWidthRemainingKB    :: Maybe Integer
    }

data PhotosetQuota
 = PhotosetQuota
    { photosetCreated   :: Integer
    , photosetRemaining :: Maybe Int  -- Nothing => unlimited/lots (pro users)
    }

data Cluster
 = Cluster
    { clusterTags  :: [Tag]
    , clusterCount :: Int
    }

data TagDetails
 = TagDetails
    { tagDetailsId     :: TagID
    , tagDetailsAuthor :: UserID
    , tagDetailsRaw    :: [String]
    , tagDetailsName   :: String
    , tagDetailsCount  :: Maybe Int
    , tagDetailsScore  :: Maybe Int
    }

data URLDetails
 = URLDetails
    { urlDetailsURL  :: URLString
    , urlDetailsType :: String
    }

data Filter = Friends | Family | Both | Neither

instance Show Filter where
  show x = 
    case x of
      Friends -> "friends"
      Family  -> "family"
      Both    -> "both"
      Neither -> "neither"

data Contact
 = Contact
     { conId       :: String
     , conUser     :: User
     , conIcon     :: Maybe Bool
     , conIsFriend :: Maybe Bool
     , conIsFamily :: Maybe Bool
     , conIgnored  :: Maybe Bool
     }

data SizeDetails
 = SizeDetails 
     { sizeDetailsLabel  :: String
     , sizeDetailsWidth  :: Int
     , sizeDetailsHeight :: Int
     , sizeDetailsSource :: URLString
     , sizeDetailsURL    :: URLString
     }

type NameContext = ResContext Namespace

data Namespace
 = Namespace
     { namespaceUsage  :: Integer
     , namespacePreds  :: Integer
     , namespaceName   :: String
     }

data ResContext a
 = ResContext
     { resCtxtPage    :: Maybe Int
     , resCtxtPages   :: Maybe Int
     , resCtxtPerPage :: Maybe Int
     , resCtxtTotal   :: Maybe Int
     }

type TagMode = String

data MachineTagPair
 = MachineTagPair
     { mtPairNamespace :: String
     , mtPairPredicate :: String
     , mtPairUsage     :: Integer
     , mtPairName      :: String
     }

data MachineTagPred
 = MachineTagPred
     { mtPredNamespaces :: Integer
     , mtPredUsage      :: Integer
     , mtPredName       :: String
     }

data MachineTag
 = MachineTag
     { mTagNamespace :: String
     , mTagPredicate :: String
     , mTagUsage     :: Integer
     , mTagValue     :: String
     }

data DateDetails
 = DateDetails 
     { dateMinTaken  :: Maybe DateString
     , dateMaxTaken  :: Maybe DateString
     , dateMinUpload :: Maybe DateString
     , dateMaxUpload :: Maybe DateString
     }

nullDateDetails :: DateDetails
nullDateDetails = DateDetails
     { dateMinTaken  = Nothing
     , dateMaxTaken  = Nothing
     , dateMinUpload = Nothing
     , dateMaxUpload = Nothing
     }
     
data TagInfo
 = TagInfo
     { tagName  :: String
     , tagCount :: Maybe Int
     }

nullTagInfo :: TagInfo
nullTagInfo = TagInfo
    { tagName  = ""
    , tagCount = Nothing
    }
