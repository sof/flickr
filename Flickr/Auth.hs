--------------------------------------------------------------------
-- |
-- Module      : Flickr.Auth
-- Description : flickr.auth - authentication, flickr-style.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Binding to flickr.auth's (signed) API
--------------------------------------------------------------------
module Flickr.Auth where

import Flickr.Types
import Flickr.Types.Import
import Flickr.Monad

-- | Returns a frob to be used during authentication. 
-- This method call must be signed.
getFrob :: FM AuthFrob
getFrob = signedMethod $ do
  flickTranslate toAuthFrob $
    flickCall "flickr.auth.getFrob" []

-- | Returns the credentials attached to an authentication token. 
-- This call must be signed as specified in the authentication API spec.
checkToken :: AuthTokenValue -> FM AuthToken
checkToken tok = signedMethod $ do
  flickTranslate toAuthToken $
    flickCall "flickr.auth.checkToken" [("auth_token", tok)]

getFullToken :: AuthMiniToken -> FM AuthToken
getFullToken mtok = signedMethod $ do
  flickTranslate toAuthToken $
    flickCall "flickr.auth.getFullToken" [("mini_token", mtok)]

-- | Returns the auth token for the given frob, if one has been 
-- attached. This method call must be signed.
getToken :: AuthFrob -> FM AuthToken
getToken frob = signedMethod $ do
  flickTranslate toAuthToken $
   flickCall "flickr.auth.getToken" [("frob", aFrob frob)]

