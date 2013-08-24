module Main(main) where

import Flickr.API
import Flickr.Photos.Upload as Photos

import System.IO
import System.Environment

import Util.Authenticate
import Util.Keys

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

getAToken :: [String] -> FM (AuthToken, [String], APIKey)
getAToken ls = do
   case ls of
     ("-t":x:ls1) -> do
       liftIO $ putStrLn ("Using mini-token: " ++ x)
       (Just (_, mkT)) <- authenticateForMobile "write"
       tok <- mkT x
       liftIO $ putStrLn ("Token: " ++ authToken tok)
       return (tok, ls1, hsflickr_mobile_key)
     _ -> do
       (_,tok) <- withAPIKey hsflickr_web_key $ loginUser "write"
       liftIO $ putStrLn ("Token: " ++ authToken tok)
       return (tok, ls, hsflickr_web_key)

main :: IO ()
main = flick $ do
  ls0          <- liftIO $ getArgs
  (tok, ls,ak) <- getAToken ls0
  case ls of
    (photo_file:title:desc:tags) -> withAPIKey ak $ withAuthToken (authToken tok) $ do
       pid <- Photos.uploadPhoto photo_file (Just title) (Just desc) tags nullUploadAttr
       liftIO $ putStrLn ("Photo ID of uploaded photo: " ++ pid)
    _ -> liftIO $ do
      p <- getProgName
      putStrLn ("Usage: " ++ p ++ " [-t mini-token] upload_file title description [tag]*")
  
  
