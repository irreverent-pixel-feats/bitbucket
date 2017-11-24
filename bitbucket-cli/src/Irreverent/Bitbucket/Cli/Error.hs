{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Cli.Error
-- Copyright    : (C) 2017 - 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Cli.Error (
  -- * Types
    CliError(..)
  , renderCliError
  ) where

import Irreverent.Bitbucket.Http.Error (BitbucketAPIError, renderBitbucketAPIError)

import qualified Ultra.Data.Text as T

import Preamble

data CliError =
  BitbucketAPIFail !BitbucketAPIError
  deriving (Show)

renderCliError :: CliError -> T.Text
renderCliError (BitbucketAPIFail e) = renderBitbucketAPIError e

