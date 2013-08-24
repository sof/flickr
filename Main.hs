module Main (main) where

import Flickr.Types
import Flickr.Monad
import Flickr.People as People
import Flickr.Photos as Photos
import Flickr.URLs ( userPhotoURL )
import Flickr.Photos.Upload as Photos

import System.IO
import System.Environment

import Util.Authenticate

-- assume you have set the api_key and secret..but no token.
loginUser :: String -> FM (URLString, AuthToken)
loginUser p = do
  (Just (u, mkT)) <- authenticateForWeb p
  liftIO (putStrLn ("Authorization URL: " ++ u))
  liftIO (putStrLn ("Hit return after user has authorized call"))
  liftIO (hFlush stdout)
  liftIO getLine
  tok <- mkT
  return (u, tok)

main :: IO ()
main = flick $ do
  tok <- do
     ls <- liftIO $ getArgs
     case ls of
       [] -> do
        (_,tok) <- loginUser "read"
	liftIO $ putStrLn ("Token: " ++ authToken tok)
	return tok
       (x:_) -> do
        liftIO $ putStrLn ("Frob: " ++ x)
        (Just (u, mkT)) <- authenticateForMobile "write"
	tok <- mkT x
	liftIO $ putStrLn ("Token: " ++ authToken tok)
	return tok

  let uid = userId $ authUser tok
  liftIO (putStrLn uid)
  withAuthToken (authToken tok) $ do
    ps <- People.getPublicPhotos uid Nothing []
    liftIO $ putStrLn ("Public photos: " ++ show (length ps))
    tok <- getAuthToken
    mapM_ (\ p -> tryFlick $ Photos.addTags (photoId p) ["sigbjorn"]) ps
    u <- People.getInfo uid True
    liftIO $ putStrLn ("User name: " ++ userName u)
    liftIO $ mapM_ ( \ x -> putStrLn (userPhotoURL u (photoId x))) ps
    pid <- Photos.uploadPhoto "c:/tmp/photo.jpg" (Just "sof_portrait") (Just "test_upload")
                              ["sigbjorn"] nullUploadAttr
    liftIO (print pid)
    return ()

{-
  (x, ps) <- find "/United+States/Oregon/Portland"
  liftIO $ putStrLn ("Count: " ++ show (placeTotal x))
  liftIO $ mapM_ (putStrLn . placeURL) ps
  liftIO (restGet "http://www.flickr.com/places/United+States/Oregon/Portland" [] >>= putStrLn)
-}
  return ()
  
