module Main(main) where

import Flickr.API
import Util.Keys ( hsflickrAPIKey )

import Flickr.Photos as Photos
import System.Environment

searchPhotos :: String -> FM [PhotoDetails]
searchPhotos q = do
  (_, ps) <- Photos.search Nothing nullSearchConstraints{s_text=Just q} []
  mapM (\ x -> Photos.getInfo (photoId x) Nothing) ps

main :: IO ()
main = do
  ls <- getArgs
  case ls of
    [] -> do
      prg <- getProgName
      putStrLn ("Usage: " ++ prg ++ " search-term")
    (x:_) -> do
      ps <- flickAPI hsflickrAPIKey $ withPageSize 20 $ searchPhotos x
      putStrLn ("Search results for: " ++ x)
      mapM_ (\ p -> putStrLn (photoTitle (photoDetailsPhoto p) ++ " = " ++ getPhotoURL p)) ps
