--------------------------------------------------------------------
-- |
-- Module      : Flickr.Test
-- Description : flickr.test - testing auth and method calls.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer  : Sigbjorn Finne <sof@forkIO.com>
-- Stability   : provisional
-- Portability : portable
--
-- flickr.test API, kicking the tires for method calls and auth
-- of these.
--------------------------------------------------------------------
module Flickr.Test where

import Flickr.Monad
import Flickr.Types
import Flickr.Types.Import
import Flickr.Utils

import Text.XML.Light

-- | A testing method which echo's all parameters back in the response.
echo :: [(String,String)] -> FM [(String,String)]
echo args =
  flickTranslate toRes $
    flickrCall "flickr.test.echo" args
 where
  toRes s = parseDoc eltRes s

  eltRes e = do
    let es = children e
    return (map (\ x -> (qName (elName x), strContent x)) es)


-- | A testing method which checks if the caller is logged in then returns their username.
login :: FM User
login = withReadPerm $
  flickTranslate toUser $
    flickrCall "flickr.test.login" []

-- | Null test
nullTest :: FM ()
nullTest = withReadPerm $ postMethod $
  flickCall_ "flickr.test.null" []

