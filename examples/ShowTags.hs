{-
  List machine tag info example.
-}
module Main(main) where

import Flickr.API
import Util.Keys ( hsflickrAPIKey )

import Flickr.MachineTags as MT
import System.Environment

getTags :: String -> String -> FM [MachineTag]
getTags ns pre = do
  (_, xs) <- MT.getValues ns pre
  return xs

getPreds :: Maybe String -> FM [MachineTagPred]
getPreds ns = do
  (_, xs) <- MT.getPredicates ns
  return xs

getNSes :: Maybe String -> FM [Namespace]
getNSes mbP = do
  (_, xs) <- MT.getNamespaces mbP
  return xs

main :: IO ()
main = do
  ls <- getArgs
  case ls of
    [] -> do
      prg <- getProgName
      putStrLn ("Usage: " ++ prg ++ " {+[pred],-,namespace} [predicate]")
    [ns] -> do
      (l,ps) <- flickAPI hsflickrAPIKey $ 
             case ns of
	       ('+':pr) -> do
	         rs <- getNSes (if null pr then Nothing else Just pr) >>= return.(map namespaceName)
		 return ("namespaces with predicate: " ++ ns, rs)
	       _   -> do
	         rs <- getPreds (if ns == "-" then Nothing else Just ns) >>= return.(map mtPredName)
		 return ("predicates for NS: " ++ ns, rs)
      putStrLn ("Machine tag " ++ l)
      mapM_ (\ p -> putStrLn (' ':p)) ps
    (ns:pre:_) -> do
      ps <- flickAPI hsflickrAPIKey $ getTags ns pre
      putStrLn ("Machine tags for (NS,pred) pair: " ++ show (ns,pre))
      mapM_ (\ p -> putStrLn (' ':mTagValue p)) ps

  
