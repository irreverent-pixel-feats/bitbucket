{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Core.Data.Repository
-- Copyright    : (C) 2017 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Core.Data.Repository (
  -- * Types
    Repository(..)
  ) where

import Irreverent.Bitbucket.Core.Data.Common

import Preamble
import Prelude (Integer)

-- |
-- @"type": "repository"@
--
data Repository = Repository {
    repoCreator     :: !(Maybe User)
  , repoLanguage    :: !Language
  , repoProject     :: !(Maybe Project)
  , repoFullName    :: !RepoName
  , repoPrivacy     :: !Privacy
  , repoUuid        :: !Uuid
  , repoForkPolicy  :: !ForkPolicy
  , repoHasIssues   :: !HasIssues
  , repoDescription :: !RepoDescription
  , repoWebsite     :: !(Maybe Website)  -- ^ apparently this comes as null sometimes...
  , repoCreatedOn   :: !BitbucketTime
  , repoSize        :: !Integer
  , repoLastUpdated :: !(Maybe BitbucketTime)
  , repoHasWiki     :: !HasWiki
  , repoScm         :: !Scm
  , repoPRs         :: !Href
  , repoDownloads   :: !Href
  , repoForks       :: !Href
  , repoHooks       :: !Href
  , repoAvatar      :: !Href
  , repoHtml        :: !Href
  , repoSelf        :: !Href
  , repoCloneHttps  :: !Href
  , repoCloneSsh    :: !Href
  , repoCommits     :: !Href
  , repoTags        :: !Href
  , repoBranches    :: !Href
  , repoWatchers    :: !Href
  , repoName        :: RepoName
  } deriving (Show, Eq)
