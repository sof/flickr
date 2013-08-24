{-# OPTIONS_GHC -XExistentialQuantification -XDeriveDataTypeable #-}
--------------------------------------------------------------------
-- |
-- Module      : Flickr.Monad
-- Description : Monadic layer for supporting flickr.com interactions.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Monadic layer for supporting flickr.com interactions.
--------------------------------------------------------------------
module Flickr.Monad where

import Util.Fetch
import Util.Keys ( hsflickrAPIKey, APIKey(..) )
import Util.MD5  ( md5sumStr )
import Data.List ( intercalate, sortBy )
import Util.Post

import Text.XML.Light as XML
import Text.XML.Light.Proc as XML
import Flickr.Utils
import Control.Exception as CE
import System.FilePath as FilePath ( takeExtension )

import System.IO as IO
import Data.Typeable

data FM a = FM (FMEnv -> IO a)

data FMEnv
 = FMEnv
    { fm_api_key         :: APIKey
    , fm_is_signed       :: Bool
    , fm_post_method     :: Bool
    , fm_per_page        :: Maybe Int
    , fm_page            :: Maybe Int
    , fm_is_paged        :: Bool
    , fm_include_props   :: Maybe [String] -- Just ls => when applicable, only return properties in 'ls'.
    , fm_perm_level      :: Maybe String
    , fm_auth_token      :: Maybe String
    , fm_auth_mini_token :: Maybe String
    , fm_api_base        :: Maybe String
    }

instance Monad FM where
  return x = FM (\ _ -> return x)
  (FM a) >>= k = FM $ \ env -> do
      v <- a env
      case k v of
        (FM b) ->  b env
    
liftIO :: IO a -> FM a
liftIO x = FM (\ _ -> x)

flick :: FM a -> IO a
flick a = flickAPI hsflickrAPIKey a

flickAPI :: APIKey -> FM a -> IO a
flickAPI ak fm = 
 case (handleFlickr (\ err -> liftIO (print err) >> liftIO (throwIO (toException err)))
                    fm) of
   FM flickr_action_and_stuff -> flickr_action_and_stuff initEnv
 where
  initEnv = 
    FMEnv
      { fm_api_key         = ak
      , fm_is_signed       = False
      , fm_post_method     = False
      , fm_per_page        = Nothing
      , fm_page            = Nothing
      , fm_perm_level      = Nothing
      , fm_is_paged        = False
      , fm_include_props   = Nothing
      , fm_auth_token      = Nothing
      , fm_auth_mini_token = Nothing
      , fm_api_base        = Nothing
      }

withAPIKey :: APIKey -> FM a -> FM a
withAPIKey ak (FM x) = FM (\ env -> x env{fm_api_key=ak})

getAPIKey :: FM APIKey
getAPIKey = FM (\ env -> return (fm_api_key env))

withSharedSecret :: String -> FM a -> FM a
withSharedSecret s (FM x) = FM ( \ env -> x env{fm_api_key=(fm_api_key env){apiSecret=s}})

withAuthToken :: String -> FM a -> FM a
withAuthToken s (FM x) = FM ( \ env -> x env{fm_auth_token=Just s})

withAuthMiniToken :: String -> FM a -> FM a
withAuthMiniToken s (FM x) = FM ( \ env -> x env{fm_auth_mini_token=Just s})

getAuthToken :: FM String
getAuthToken = FM (\env -> return (fromMaybe "" (fm_auth_token env)))

getAuthMiniToken :: FM String
getAuthMiniToken = FM (\env -> return (fromMaybe "" (fm_auth_mini_token env)))

withPageSize :: Int -> FM a -> FM a
withPageSize sz (FM x) = FM (\ env -> x env{fm_is_paged=True,fm_per_page=Just sz})

onlyTheseProperties :: [String] -> FM a -> FM a
onlyTheseProperties ps (FM x) = FM (\ env -> x env{fm_include_props=Just ps})

pagedCall :: Maybe Int -> FM a -> FM a
pagedCall mbPg (FM x) = FM (\ env -> x env{fm_is_paged=True,fm_page=mbPg})

signedMethod :: FM a -> FM a
signedMethod (FM x) = FM (\ env -> x env{fm_is_signed=True})

withReadPerm  :: FM a -> FM a
withReadPerm (FM x) = FM (\ env -> x env{fm_is_signed=True, fm_perm_level=Just "read"})

withWritePerm :: FM a -> FM a
withWritePerm (FM x) = FM (\ env -> x env{fm_is_signed=True, fm_perm_level=Just "write"})

withDeletePerm :: FM a -> FM a
withDeletePerm (FM x) = FM (\ env -> x env{fm_is_signed=True, fm_perm_level=Just "delete"})

postMethod :: FM a -> FM a
postMethod (FM x) = FM (\ env -> x env{fm_post_method=True})

withBase :: URLString -> FM a -> FM a
withBase b (FM x) = FM (\ env -> x env{fm_api_base=Just b})

getBaseURL :: FM URLString
getBaseURL = FM ( \ env -> return (fromMaybe api_base (fm_api_base env)) )

getMobileAuthURL :: FM URLString
getMobileAuthURL = FM (\ env -> return (fromMaybe "" (fm_api_base env)))

api_base :: URLString
api_base = "http://api.flickr.com/services/rest/"

auth_base :: URLString
auth_base = "http://api.flickr.com/services/auth/?"

upload_base :: URLString
upload_base = "http://api.flickr.com/services/upload/"

replace_base :: URLString
replace_base = "http://api.flickr.com/services/replace/"

flickTranslate :: (String -> ErrM a)
               -> FM String
	       -> FM a
flickTranslate f a = do
  x <- a
  case f x of
    Left  e -> {-liftIO (print e) >> -}liftIO (throwIO (toException e))
    Right r -> return r

flickCall_ :: String -> [(String,String)] -> FM ()
flickCall_ m args = flickTranslate (checkResponse) (flickCall m args) >> return ()

mbArg :: String -> Maybe String -> [(String,String)] -> [(String,String)]
mbArg _ Nothing xs = xs
mbArg t (Just a) xs = (t,a):xs

eiArg :: String
      -> String
      -> Either String String
      -> [(String,String)]
      -> [(String,String)]
eiArg t _ (Left x)  xs = (t,x):xs
eiArg _ t (Right x) xs = (t,x):xs

lsArg :: String -> [String] -> [(String,String)] -> [(String,String)]
lsArg _ [] xs = xs
lsArg t ls xs = (t,intercalate "," ls):xs

flickrCall :: String -> [(String,String)] -> FM String
flickrCall m args = flickCall m args

flickCall :: String -> [(String,String)] -> FM String
flickCall m args = FM $ \ env -> do
  let
     mb _ Nothing = []
     mb x (Just v) = [(x,show v)]
     
     pgContext = fm_is_paged env
     
     includeProps ls = 
       maybe ls
             (\ x -> ("include",intercalate "," x):ls)
	     (fm_include_props env)

     pageContext ls
      | not pgContext = ls
      | otherwise = ls ++ concat [ mb "per_page" (fm_per_page env)
                                 , mb "page"     (fm_page env)
			         ]
     isSigned xs
      | fm_is_signed env = xs ++ [("api_sig",api_sig)]
      | otherwise = ("format", "rest") : xs
    
     withAToken xs 
      | fm_is_signed env =
        case fm_auth_token env of
          Nothing -> xs
	  Just x  -> 
	     -- if auth_token is already an arg (cf. flickr.auth.checkToken),
	     -- don't add a second one. ToDo: issue a warning/heads-up about
	     -- this ?
	    case lookup "auth_token" xs of
	      Nothing -> ("auth_token",x):xs
	      _ -> xs
      | otherwise = xs

     withPerms xs = 
       case fm_perm_level env of
         Nothing -> xs
	 Just x  -> 
	   case fm_auth_token env of
	     Just{} -> xs
	     _     -> ("perms",x):xs
     
     withMethod xs = 
       case m of
         "" -> xs
	 _  -> ("method",m):xs

     withNoFiles xs = 
       filter (\ (_,y) -> case y of { '@':_ -> False; _ -> True }) xs

     api_sig_inp = apiSecret (fm_api_key env) ++ 
                   concatMap (\ (x,y) -> x++y) (
                   sortBy (\ a b -> compare (fst a) (fst b))
		         (("api_key", apiKey $ fm_api_key env) : 
			  withMethod (withAToken (withPerms $ withNoFiles args))))

     api_sig = md5sumStr api_sig_inp
               
     restMeth
      | fm_post_method env = restPost
      | otherwise          = restGet

--  print ("XX",args,fm_is_signed env, fm_auth_token env, api_sig_inp)
  restMeth (fromMaybe api_base (fm_api_base env))
           (includeProps $
	    pageContext  $
            (withMethod 
              (("api_key", apiKey $ fm_api_key env) : 
	          isSigned (withAToken $ withPerms args))))
			  
mkLoginURL :: String -> String -> FM String
mkLoginURL fr p = FM $ \ env -> do
  return (genLoginURL (apiKey $ fm_api_key env) (apiSecret $ fm_api_key env)
                      fr p)        

genLoginURL :: String
            -> String
	    -> String
	    -> String
	    -> String
genLoginURL api_key secret frob perm = 
  auth_base ++ "api_key="++api_key ++ "&frob="++frob ++
              "&perms="++perm++"&api_sig="++api_sig
 where
  api_sig = md5sumStr $ secret ++ 
                        "api_key" ++ api_key ++ 
                        "frob" ++ frob ++ 
                        "perms" ++ perm

restGet :: {-URL-}String -> [(String,String)] -> IO String
restGet a kv = do
--debug:  print (a ++ wArgs kv)
  readContentsURL (a ++ wArgs kv)
 where
   wArgs [] = ""
   wArgs xs = '?':intercalate "&" (map (\ (k,v) -> k ++ '=':v) xs)
  
restPost :: {-URL-}String -> [(String,String)] -> IO String
restPost a kv = do
--debug:  print (a ++ wArgs kv)
   -- ToDo: fix, the encoding of arguments that 'toRequest' performs
   -- interacts badly with signature computation. Signatures need to be
   -- done over the encoded strings, it seems.
  (vs2,hs0,body0) <- toRequest r (Just PostQuery)
  let body = case body0 of { "" -> ""; _ -> '\r':'\n':body0}
  let hs = ("Content-Length",show (length body)):hs0
  let vs  = wArgs vs1 ++ (if null vs2 then "" else if null vs1 then '?':vs2 else '&':vs2)
-- debug:  print (a++vs,hs,body)
  postContentsURL (a++vs) hs body
 where
   r0 = newPostRequest "flickr"
   (r,vs1)  = foldr (\ (x,y) (acc,hs) -> 
                  case y of
		    '@':xs -> (addNameFile x xs (extToTy xs) acc,hs)
		    _      -> (addNameValue x y acc, hs))
	      (r0,[])
	      kv

   wArgs [] = ""
   wArgs xs = '?':intercalate "&" (map (\ (k,v) -> k ++ '=':v) xs)
   
    -- ToDo: write/plug into general mime.types-like package here.
   extToTy fp = 
     case FilePath.takeExtension fp of
       ""     -> Just "image/jpeg"
       ".gif" -> Just "image/gif"
       ".jpg" -> Just "image/jpeg"
       ".png" -> Just "image/png"
       _      -> Just "image/jpeg"
  
type ErrM a = Either FlickErr a

data FlickErr
 = FlickErr
     { flickErrorCode   :: Int
     , flickErrorType   :: FlickErrorType
     , flickErrorMsg    :: String
     , flickErrorLoc    :: Maybe String
     , flickErrorSource :: String
     } deriving Typeable

data SomeFlickException = forall e . Exception e => SomeFlickException e 
    deriving Typeable 

instance Show SomeFlickException where 
    show (SomeFlickException e) = show e 

instance Exception SomeFlickException 

flickToException :: Exception e => e -> SomeException 
flickToException = toException . SomeFlickException 

flickFromException :: Exception e => SomeException -> Maybe e 
flickFromException x = do 
    SomeFlickException a <- fromException x 
    cast a 

instance Exception FlickErr where
  toException = flickToException
  fromException = flickFromException

handleFlickr :: (FlickErr -> FM a) -> FM a -> FM a
handleFlickr h e = catchFlickr e h

tryFlick :: FM a -> FM (Either FlickErr a)
tryFlick f = handleFlickr (\ x -> return (Left x)) (f >>= return.Right)

throwFlickErr :: FlickErr -> FM a
throwFlickErr e = FM (\ _ -> throwIO e)

catchFlickr :: FM a -> (FlickErr -> FM a) -> FM a
catchFlickr (FM f) hdlr = FM $ \ env -> 
  CE.catch (f env) 
           (\ e1 -> case hdlr e1 of { (FM act) -> act env })

instance Show FlickErr where
  show x = unlines (
   [ "Flickr error:"
   , ""
   , " Code: " ++ show (flickErrorCode x)
   , " Type: " ++ show (flickErrorType x)
   , " Details: " ++ flickErrorMsg x
   ] ++ (if flickErrorType x == IllformedError  || flickErrorType x == FlickParseError
          then [" Source: " ++ flickErrorSource x]
	  else []))

flickError :: FlickErr
flickError 
 = FlickErr
     { flickErrorCode   = (-1)
     , flickErrorType   = UnexpectedResponse
     , flickErrorMsg    = ""
     , flickErrorLoc    = Nothing
     , flickErrorSource = ""
     }

data FlickErrorType
    -- fatal errors while processing response payloads:
 = UnexpectedResponse -- not a 'rsp'
 | MissingStatus      -- no 'stat' on 'rsp' element
 | EmptyResponse
 | IllformedError
 | FlickParseError
 | FlickrAPIError     -- see 'code' + 'msg' field for details
   deriving ( Eq )

instance Show FlickErrorType where
  show x = 
   case x of
      UnexpectedResponse -> "unexpected XML Flickr response"
      MissingStatus      -> "unexpected response; missing 'rsp' top element"
      EmptyResponse      -> "empty 'rsp' content"
      IllformedError     -> "ill-formed 'rsp' content"
      FlickParseError    -> "ok response returned, but ill-formed"
      FlickrAPIError     -> "Flickr API error"

parseDoc :: (Element -> Maybe a)
         -> String
	 -> ErrM a
parseDoc f s = 
  case checkResponse s of
    Left err -> Left err
    Right x  ->
     case f x of
        Nothing -> 
         Left flickError{ flickErrorType   = FlickParseError
	                , flickErrorSource = show (length s) ++ '\n':s
	   	        }
        Just res -> Right res

checkResponse :: String -> ErrM Element
checkResponse s = 
  case parseXMLDoc s of
    Nothing -> Left flickError { flickErrorSource = s
                               , flickErrorType   = UnexpectedResponse
			       }
    Just e  
     | elName e /= nsName "rsp" -> Left flickError{flickErrorSource=s}
     | otherwise -> 
	  case pAttr "stat" e of
	    Nothing -> Left flickError{ flickErrorType   = MissingStatus
	                              , flickErrorSource = s
				      }
	    Just "ok" -> 
	      case elChildren e of
	        [] -> Right blank_element
		(x:_) -> Right x
	    Just "fail" -> 
	      case findChild (nsName "err") e of
	        Nothing -> 
		  Left flickError{ flickErrorType   = IllformedError
		                 , flickErrorSource = s
				 }
		Just e1 -> 
		  case pAttr "code" e1 of
		    Nothing -> 
		      Left flickError{ flickErrorSource = s
		                     , flickErrorType   = IllformedError
				     }
		    Just v_str  -> 
		      case reads v_str of
		       ((v,_):_) -> Left 
		          flickError { flickErrorType = FlickrAPIError
			             , flickErrorCode = v
				     , flickErrorSource = s
				     , flickErrorMsg  = 
				         fromMaybe "" (pAttr "msg" e1)
			             }
                       _ -> Left flickError{ flickErrorSource = s
	                                   , flickErrorType   = IllformedError
				           }
            _ -> Left flickError{ flickErrorSource = s
	                        , flickErrorType   = IllformedError
				}
