{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Http.Common
-- Copyright    : (C) 2017 - 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Http.Common (
  -- * Values
    baseURL
  , baseV2URL
  , baseReq
  ) where

import Ultra.Control.Lens ((&), (.~))
import qualified Ultra.Data.Text as T

import qualified Network.Wreq as W

import Preamble

baseURL :: T.Text
baseURL = "https://api.bitbucket.org/"

baseV2URL :: T.Text
baseV2URL = baseURL <> "2.0"

baseReq :: W.Options
baseReq = W.defaults
  & (W.redirects .~ 10)
  . (W.checkStatus .~ Nothing)
