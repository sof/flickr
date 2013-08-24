--------------------------------------------------------------------
-- |
-- Module    : Util.Keys
-- Copyright : (c) Sigbjorn Finne, 2008
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: so-so
--
-- hs-flickr keys and secrets; use and authorize with care..
--
--------------------------------------------------------------------
module Util.Keys 
       ( APIKey(..)
       , hsflickrAPIKey        -- :: APIKey

       , hsflickr_mobile_key   -- :: APIKey
       , hsflickr_web_key      -- :: APIKey
       ) where

-- Note: we use hsflickr_ prefixes to clearly indicate here, and
-- whereever they are used, the provenance of these keys.

data APIKey
 = APIKey
      { apiKind    :: String -- desktop,web,mobile
      , apiKey     :: String
      , apiSecret  :: String
      , apiAuthURL :: Maybe String
      }

hsflickrAPIKey :: APIKey
hsflickrAPIKey  = hsflickr_mobile_key
--hsflickrAPIKey  = hsflickr_web_key

-- | the API key currently registered for hsflickr, 'mobile' application
-- version (write perms.)
hsflickr_mobile_key :: APIKey
hsflickr_mobile_key = APIKey
      { apiKind    = "mobile"
      , apiKey     = "a714ef83577adf5ea7e9d1ea3b8f39f0"
      , apiSecret  = "49b89db22ca223b4"
      , apiAuthURL = Just "http://www.flickr.com/auth-72157608768003027"
      }

-- | the API key currently registered for hsflickr, web application
-- version (write perms.)
hsflickr_web_key :: APIKey
hsflickr_web_key = APIKey
      { apiKind    = "web"
      , apiKey     = "9978c9501bd9970804605a320052cea1"
      , apiSecret  = "41a5ec838a12a388" 
      , apiAuthURL = Just "http://api.flickr.com/services/auth/?"
      }


