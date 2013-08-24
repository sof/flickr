--------------------------------------------------------------------
-- |
-- Module    : Util.Fetch
-- Copyright : (c) Sigbjorn Finne, 2008
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: so-so
-- 
-- Simple GET\/de-ref of URLs; abstracting out networking backend\/package.
--
module Util.Fetch 
       ( readContentsURL
       , readUserContentsURL
       
       , postContentsURL

       , URLString
       , User(..)
       ) where
       
--import Network.Curl
import Network.Browser
import Network.HTTP
import Network.URI

type URLString = String

data User
 = User { userName :: String
        , userPass :: String
	}

readContentsURL :: URLString -> IO String
readContentsURL u = do
  req <- 
    case parseURI u of
      Nothing -> fail ("ill-formed URL: " ++ u)
      Just ur -> return (defaultGETRequest ur)
    -- don't like doing this, but HTTP is awfully chatty re: cookie handling..
  let nullHandler _ = return ()
  (_u, resp) <- browse $ setOutHandler nullHandler >> request req
  case rspCode resp of
    (2,_,_) -> return (rspBody resp)
    _ -> fail ("Failed reading URL " ++ show u ++ " code: " ++ show (rspCode resp))

{- Curl version:
readContentsURL :: URLString -> IO String
readContentsURL u = do
  let opts = [ CurlFollowLocation True
	     ]
  (_,xs) <- curlGetString u opts
  return xs
-}

readUserContentsURL :: User -> URLString -> IO String
readUserContentsURL usr us = do -- readContentsURL u
  req <- 
    case parseURI us of
      Nothing -> fail ("ill-formed URL: " ++ us)
      Just ur -> return (defaultGETRequest ur)
    -- don't like doing this, but HTTP is awfully chatty re: cookie handling..
  let nullHandler _ = return ()
  (u, resp) <- browse $ do 
                  setOutHandler nullHandler
                  setAllowBasicAuth True
		  setAuthorityGen (\ _ _ -> return (Just (userName usr,userPass usr)))
{-
                  addAuthority AuthBasic{ auUsername = userName usr
                                        , auPassword = userPass usr
                                        , auRealm    = ""
                                        , auSite     = nullURI{uriPath="/"}
                                        }
-}
                  request req
  case rspCode resp of
    (2,_,_) -> return (rspBody resp)
    _ -> fail ("Failed reading URL " ++ show u ++ " code: " ++ show (rspCode resp))


postContentsURL :: URLString -> [(String,String)] -> String -> IO String
postContentsURL u hdrs body = do
  let hs = 
       case parseHeaders $ map (\ (x,y) -> x++": " ++ y) hdrs of
         Left{} -> []
	 Right xs -> xs
  req0 <- 
    case parseURI u of
      Nothing -> fail ("ill-formed URL: " ++ u)
      Just ur -> return (defaultGETRequest ur)
  let req = req0{rqMethod=POST
                ,rqBody=body
		,rqHeaders=hs
	        }
  let nullHandler _ = return ()
  (_,rsp) <- browse $ setOutHandler nullHandler >> request req
  case rspCode rsp of
    (2,_,_) -> return (rspBody rsp)
    x -> fail ("POST failed - code: " ++ show x ++ ", URL: " ++ u)

{- Curl versions:
readUserContentsURL :: User -> URLString -> IO String
readUserContentsURL u url = do
  let opts = [ CurlHttpAuth [HttpAuthAny]
             , CurlUserPwd (userName u ++ 
	                    case userPass u of {"" -> ""; p -> ':':p })
             , CurlFollowLocation True
	     ] 
  (_,xs) <- curlGetString url opts
  return xs
     
postContentsURL :: URLString -> [(String,String)] -> String -> IO String
postContentsURL u hdrs body = do
  let opts = [ CurlCustomRequest "POST"
             , CurlFollowLocation True
	     , CurlPost True
	     , CurlPostFields [body]
	     , CurlHttpTransferDecoding False
	     ] ++ [CurlHttpHeaders (map ( \ (x,y) -> (x ++ ':':y)) hdrs)]
  rsp <- curlGetResponse u opts
  case respStatus rsp `div` 100 of
    2 -> return (respBody rsp)
    x -> fail ("POST failed - code: " ++ show x ++ ", URL: " ++ u)

-}
