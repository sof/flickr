module Util.Post where

import Codec.MIME.Type as MIME
import Codec.MIME.Parse as MIME
import Util.MIME
import Codec.URLEncoder

import Data.List
import System.Random
import Numeric

-- ease the working with POST requests and their
-- outgoing payloads.

data PostReq
 = PostReq
    { prName :: String
    , prVals :: [PostParam]
    }

data PostKind
 = PostQuery
 | PostWWWForm
 | PostFormData

newPostRequest :: String -> PostReq
newPostRequest s = PostReq{prName=s,prVals=[]}

testRequest :: PostReq
            -> Maybe PostKind
	    -> IO ()
testRequest a b = do
  (as,bs,cs) <- toRequest a b
  putStrLn ("URL query portion: " ++ as)
  putStrLn (unlines $ map (\ (k,v) -> k ++ ':':' ':v) bs)
  putStrLn ""
  putStrLn cs
  

toRequest :: PostReq
          -> Maybe PostKind
	  -> IO (String, [(String,String)], String)
toRequest pr mbKind = 
  case mbKind of
    Nothing -> 
      case filter isPostFile (prVals pr) of
        (_:_) -> toRequest pr (Just PostFormData)
	_ -> toRequest pr (Just PostWWWForm)
    Just PostQuery -> 
      case partition isPostFile (prVals pr) of
        (ls@(_:_),bs) -> do
	  putStrLn ("toRequest: POST request contains " ++ 
	        shows (length ls) (" files; unable to represent as query string"))
          putStrLn ("Defaulting to multiform/form-data instead")
	  toRequest pr{prVals=bs++ls} (Just PostFormData)
        _ -> return (intercalate "&" $ 
	                map (\ (PostNameValue n v) -> encodeString n ++ '=':encodeString v) (prVals pr), [],"")
    Just PostWWWForm ->
      case partition isPostFile (prVals pr) of
        (ls@(_:_),bs) -> do
	  putStrLn ("toRequest: POST request contains " ++ 
	        shows (length ls) (" files; unable to represent as application/x-www-form-urlencoded"))
          putStrLn ("Defaulting to multiform/form-data instead")
	  toRequest pr{prVals=bs++ls} (Just PostFormData)
        _ -> return ( ""
	            , [("Content-Type", "application/x-www-form-urlencoded")]
	            , crnl ++ (intercalate "&" $ 
	                 map (\ (PostNameValue n v) -> encodeString n ++ '=':encodeString v) (prVals pr))
		    )
    Just PostFormData -> do
      mv <- toMIMEValue (prVals pr)
      let (hs,bod) = showMIMEValue "" mv
      return ( "", hs, bod)

addNameValue :: String -> String -> PostReq -> PostReq
addNameValue n v pr = pr{prVals=(PostNameValue n v):prVals pr}

addNameFile :: String -> FilePath -> Maybe String -> PostReq -> PostReq
addNameFile nm fp mbTy pr = pr{prVals=(PostFile nm fp mbTy):prVals pr}

data PostParam
 = PostNameValue String    -- name
                 String    -- value (assume: un-encoded)
 | PostFile String         -- name
            FilePath       -- local file to post
	    (Maybe String) --  Just ty => use 'ty' as content-type

isPostFile :: PostParam -> Bool
isPostFile PostFile{} = True
isPostFile _ = False

toMIMEValue :: [PostParam] -> IO MIMEValue
toMIMEValue ps = do
  let low = (2^(32::Integer)-1) :: Integer
  x <- randomRIO (low,low*low)
  let boundary = replicate 30 '-' ++ showHex x ""
  let (fs,ns) = 
        case partition isPostFile ps of
	  ([_],_) -> ([],ps)
	  xs -> xs

  fns <- mapM (fromPostParam boundary) ns
  (mi,b)  <- mixedType
  ffs <- mapM (fromPostParam b) fs
  let addM [] = []
      addM xs = [mi{mime_val_content=Multi xs}]
      
  return MIMEValue
    { mime_val_type    = MIME.Type{ mimeType   = Multipart FormData
                                  , mimeParams = [("boundary", boundary)]
				  }
    , mime_val_disp    = Nothing
    , mime_val_content = Multi (fns ++ addM ffs)
    , mime_val_inc_type = True
    , mime_val_headers = []
    }

fromPostParam :: String -> PostParam -> IO MIMEValue
fromPostParam _boundary (PostNameValue n v) = 
  return MIMEValue
     { mime_val_type = MIME.Type
         { mimeType = Application "x-www-form-urlencoded"
	 , mimeParams=[]
	 }
     , mime_val_disp = Just $
         Disposition { dispType = DispFormData 
                     , dispParams = [Name n]
		     }
     , mime_val_content = Single (encodeString v)
     , mime_val_headers = []
     , mime_val_inc_type = False
     }
fromPostParam _boundary (PostFile nm fp mbTy) = do
  ty <- 
    case mbTy of
      Nothing -> getMIMEType fp
      Just ty -> toMIMEType ty
  mv <- uploadFile nm fp
  return mv{mime_val_type=ty}

toMIMEType :: String -> IO Type
toMIMEType tyStr = 
  case parseMIMEType tyStr of
    Just t -> return t
    _      -> return MIME.Type{mimeType=Text "plain",mimeParams=[]}

getMIMEType :: String -> IO Type
getMIMEType x = 
  case parseMIMEType x of
    Just t -> return t
    _      -> return MIME.Type{mimeType=Application "octet-stream",mimeParams=[]}
