{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Core.Data.RepositorySummary
-- Copyright    : (C) 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Core.Data.RepositorySummary (
  -- * Types
    RepositorySummary(..)
  ) where

import Irreverent.Bitbucket.Core.Data.Common

import Preamble

data RepositorySummary = RepositorySummary {
    rsFullName :: !RepoName
  , rsName   :: !RepoName
  , rsSelf   :: !Href
  , rsHtml   :: !Href
  , rsAvatar :: !Href
  , rsUuid   :: !Uuid
  } deriving (Show, Eq)

