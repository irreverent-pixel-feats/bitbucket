{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Core.Data.Auth
-- Copyright    : (C) 2017 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Core.Data.Auth (
  -- * Types
    Auth(..)
  ) where

import qualified Ultra.Data.Text as T

import Preamble

-- |
-- Right now only Basic Auth is supported, as
-- Bitbuckets "App passwords" are the most convenient
-- way to interact with the REST API.
--
data Auth =
  Basic !T.Text !T.Text -- ^ username, password
  deriving (Show, Eq)
