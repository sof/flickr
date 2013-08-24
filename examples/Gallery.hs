{-
  Example of how to create a picture gallery of thumbnails
  from Flickr, dividing it up into multiple HTML pages.

  The functionality is exposed as a stand-alone application
  here, but it shouldn't take much imagination to work out
  how to take the HTML-generating functions and use it as
  part of your web application (using your server-side
  Haskell wrapper of choice..)

  On the Flickr API side, it demonstrates how to use the
  search-by-tag + interestingness.

  foo$ gallery -o g.html portland
  # generate mini gallery for 'portland'-related photos.

-}
module Main(main) where

-- the base API functionality, including monad and
-- supporting types; good thing to start your hsflickr
-- import lists with
import Flickr.API
import Util.Keys ( hsflickrAPIKey, APIKey(..) )

import Flickr.Photos as Photos
import Flickr.URLs   as URL

import Text.XHtml

import System.IO
import System.Environment

searchPhotos :: String -> FM [PhotoDetails]
searchPhotos q = do
  (_, ps) <- Photos.search Nothing nullSearchConstraints{s_text=Just q} []
  mapM (\ x -> Photos.getInfo (photoId x) Nothing) ps

generateThumbs :: String -> [(URLString,URLString,String)] -> Html
generateThumbs s labs = body (
  concatHtml [ h1 (stringToHtml ("Gallery of '" ++ s ++ "' Flickr photos"))
             , table (concatHtml (map toRow rws))
	     ])
 where
  rws = splitInto 4 labs

toRow :: [(String,String,String)] -> Html
toRow xs = tr $
  concatHtml $
    map (\ (imgUrl,imgPage,ttl) ->
           td (thediv (anchor (image ! [src imgUrl]) ! [href imgPage] +++ br +++
               stringToHtml ttl) ! [align "center"] ))
        xs

main :: IO ()
main = do
  ls0 <- getArgs
  (out,ls) <-
    case ls0 of
      "-o":x:xs -> do
         h <- openFile x WriteMode
	 return (\ vs -> hPutStrLn h vs >> hFlush h, xs)
      _ -> return (putStrLn, ls0)
  case ls of
    [] -> do
      prg <- getProgName
      putStrLn ("Usage: " ++ prg ++ " search-term")
    (x:_) -> do
      putStrLn ("Please authenticate via: " ++ fromMaybe "<unknown>" (apiAuthURL hsflickrAPIKey))
      putStrLn ("Hit <return> when done so..")
      hFlush stdout
      _ <- getLine
      ps <- flickAPI hsflickrAPIKey $ withPageSize 20 $ searchPhotos x
      let lls = map (\ ph -> ( photoSourceURL ph PhotoSizeThumb
                             , getPhotoURL ph
                             , photoTitle (photoDetailsPhoto ph)
			     ))
	            ps
      out (renderHtml $ generateThumbs x lls)


splitInto :: Int -> [a] -> [[a]]
splitInto _ [] = []
splitInto x xs =
  case splitAt x xs of
   (as,[]) -> [as]
   (as,bs) -> as : splitInto x bs

