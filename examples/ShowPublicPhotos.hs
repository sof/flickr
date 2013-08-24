{-
  List the public photos for a given user; demonstrates
  the use of the flickr.people + photos APIs for accessing
  public/non-authenticated calls methods.
-}
module Main(main) where

import Flickr.API
import Util.Keys ( hsflickrAPIKey )

import Flickr.People as People
import Flickr.Photos as Photos
import System.Environment

getUserFromName :: String -> FM (Maybe User)
getUserFromName uname = do
  mb  <- tryFlick $ People.findByUsername uname
  case mb of
    Left err 
     | flickErrorCode err == 1 -> do
       liftIO $ putStrLn ("Unknown user: " ++ uname)
       return Nothing
     | otherwise -> liftIO (putStrLn (show (flickErrorCode err))) >> throwFlickErr err
    Right u -> return (Just u)

getPubPhotos :: String -> FM [PhotoDetails]
getPubPhotos uname = do
  mb  <- getUserFromName uname
  case mb of
    Nothing -> return []
    Just u  -> do
        -- only return the first ten photos:..
      ps <- withPageSize 10 $ People.getPublicPhotos (userId u) Nothing []
      liftIO $ putStrLn ("Number of public photos: " ++ show (length ps))
      mapM (\ x -> Photos.getInfo (photoId x) Nothing) ps

main :: IO ()
main = do
  ls <- getArgs
  case ls of
    [] -> do
      prg <- getProgName
      putStrLn ("Usage: " ++ prg ++ " user-name")
    (x:_) -> do
      ps <- flickAPI hsflickrAPIKey $ getPubPhotos x
      putStrLn ("Public photos for: " ++ x)
      mapM_ (\ p -> putStrLn (photoTitle (photoDetailsPhoto p) ++ " = " ++ getPhotoURL p)) ps

  
