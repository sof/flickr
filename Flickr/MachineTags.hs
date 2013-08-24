--------------------------------------------------------------------
-- |
-- Module      : Flickr.MachineTags
-- Description : flickr.machinetags - fetch photos by triple (ns,pred,val) tags.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- The flickr.machinetags API, fetching photos by their machinetags.
--------------------------------------------------------------------
module Flickr.MachineTags where

import Flickr.Monad
import Flickr.Types
import Flickr.Types.Import

import Control.Monad

-- | Return a list of unique namespaces, optionally limited by a given predicate, in alphabetical order.
getNamespaces :: Maybe String -- ^ optional predicate
              -> FM (NameContext, [Namespace])
getNamespaces mbPred = 
   flickTranslate toNamespaceList $
     flickrCall "flickr.machinetags.getNamespaces"
                (mbArg "predicate" mbPred [])

-- | Return a list of unique namespace and predicate pairs, optionally 
-- limited by predicate or namespace, in alphabetical order.
getPairs :: Maybe String  -- ^ namespace constraint
         -> Maybe String  -- ^ predicate constraint
	 -> FM (ResContext MachineTagPair, [MachineTagPair])
getPairs mbNS mbPred = 
   flickTranslate toPairList $
     flickrCall "flickr.machinetags.getPairs"
                (mbArg "predicate" mbPred $ 
		 mbArg "namespace" mbNS [])

-- | Return a list of unique predicates, optionally limited by a given namespace.
getPredicates :: Maybe String -> FM (ResContext MachineTagPred, [MachineTagPred])
getPredicates mbNS = 
   flickTranslate toPredList $
     flickrCall "flickr.machinetags.getPredicates"
                (mbArg "namespace" mbNS [])

-- | Return a list of unique values for a namespace and predicate.
getValues :: String -> String -> FM (ResContext MachineTag, [MachineTag])
getValues ns pre = liftM (\ (x,xs) -> (x, map (\ p -> p{mTagPredicate=pre,mTagNamespace=ns}) xs)) $
   flickTranslate toMachineTagList $
     flickrCall "flickr.machinetags.getValues"
                [ ("namespace", ns)
		, ("predicate", pre)
		]
