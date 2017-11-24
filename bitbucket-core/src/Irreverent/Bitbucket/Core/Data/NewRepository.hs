{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Core.Data.NewRepository
-- Copyright    : (C) 2017 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Core.Data.NewRepository (
  -- * Types
    NewRepository(..)
  ) where

import Irreverent.Bitbucket.Core.Data.Common

import Preamble

data NewRepository = NewRepository {
    newRepoDescription :: !RepoDescription
  , newRepoScm         :: !Scm
  , newRepoProject     :: !(Maybe ProjectKey)
  , newRepoForkPolicy  :: !ForkPolicy
  , newRepoPrivacy     :: !Privacy
  , newRepoLanguage    :: !Language
  , newRepoHasWiki     :: !HasWiki
  , newRepoHasIssues   :: !HasIssues
  } deriving (Show, Eq)
