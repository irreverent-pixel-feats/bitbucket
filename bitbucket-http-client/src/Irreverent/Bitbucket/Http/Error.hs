{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Http.Error
-- Copyright    : (C) 2017 - 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Http.Error (
  -- * Types
    BitbucketAPIError(..)
  -- * Functions
  , renderBitbucketAPIError
  ) where

import Irreverent.Bitbucket.Core.Data.Auth (Auth(..))

import qualified Ultra.Data.Text as T
import qualified Ultra.Network.HTTP.Client as H

import qualified Data.ByteString as BS

import qualified Network.HTTP.Simple as H hiding (setRequestIgnoreStatus)

import Preamble

data BitbucketAPIError =
    AuthError !Auth
  | HttpError !H.HttpException
  | JsonErrorception !H.JSONException
  | JsonError !T.Text
  | NotOkHttpResponse !(H.Response BS.ByteString)
    deriving (Show)

renderBitbucketAPIError :: BitbucketAPIError -> T.Text
renderBitbucketAPIError (AuthError (Basic username _)) = T.concat [
    "Authentication failed for username: "
   , username
   ]
renderBitbucketAPIError (HttpError x) = T.concat [
    "Http error: "
  , T.pack . show $ x
  ]
renderBitbucketAPIError (JsonErrorception x) = T.concat [
    "Json error: "
  , T.pack . show $ x
  ]
renderBitbucketAPIError (JsonError x) = T.concat [
    "Json error: "
  , x
  ]
renderBitbucketAPIError (NotOkHttpResponse x) = T.concat [
    "Got a Not-OK http response: "
  , T.pack . show $ x
  ]
