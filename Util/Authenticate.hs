module Util.Authenticate where

import Flickr.Monad
import Util.Keys
import Flickr.Types
import Flickr.Auth

-- | authenticate the 'web application' way; obtain
-- a so-called frob, generate a URL for the user to
-- authorize access via. Once the user has done so,
-- resolve the full token by performing action.
authenticateForWeb :: String -> FM (Maybe (URLString, FM AuthToken))
authenticateForWeb forPerm = do
  x <- getAPIKey
  case apiKind x of
    "web" -> do
       fr <- getFrob
       u  <- mkLoginURL (aFrob fr) forPerm
       return (Just (u, getToken fr))
    k -> do
      liftIO $ putStrLn ("Unexpected API key 'kind': " ++ k ++ ", expected 'web'.")
      return Nothing

-- | Authenticate the 'mobile application' way; emit
-- an authentication URL for the mobile application
-- along with an action that takes a authentication
-- 'mini-token' to resolve into a full token. The mini-token
-- is either something the application stores (as a secret),
-- or for the first time around, by having the user write
-- down the mini-token 9-digit (format is xxx-yyy-zzz) string
-- and input that to the application through its UI.
-- 
authenticateForMobile :: String -> FM (Maybe (URLString, String -> FM AuthToken))
authenticateForMobile perm = do
  x <- getAPIKey
  case apiKind x of
    "mobile" -> do
       u <- getMobileAuthURL
       return (Just (u, \ mt -> getFullToken mt))
    k -> do
      liftIO $ putStrLn ("Unexpected API key 'kind': " ++ k ++ ", expected 'mobile'.")
      return Nothing
