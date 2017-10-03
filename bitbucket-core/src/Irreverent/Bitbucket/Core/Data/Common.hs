{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Core.Data.Common
-- Copyright    : (C) 2017 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Core.Data.Common (
  -- * Types
    BitbucketTime(..)
  , DisplayName(..)
  , EmailMailingList(..)
  , Language(..)
  , HasIssues(..)
  , HasWiki(..)
  , Href(..)
  , Scm(..)
  , User(..)
  , Username(..)
  , UserType(..)
  , Uri(..)
  , ForkPolicy(..)
  , MqOf(..)
  , MqType(..)
  , Privacy(..)
  , Project(..)
  , ProjectKey(..)
  , ProjectName(..)
  , ReadWriteMode(..)
  , RepoDescription(..)
  , RepoName(..)
  , RepoSlug(..)
  , RepoState(..)
  , Uuid(..)
  , Website(..)
  ) where

import qualified Ultra.Data.Text as T

import Data.Time.Clock (UTCTime)

import Preamble

-- In v2 of the bitbucket API, this seems like a pretty compliant ISO8601 datetime
newtype BitbucketTime = BitbucketTime { bitbucketTime :: UTCTime} deriving (Show, Eq)

newtype Username = Username { getUsername :: T.Text } deriving (Show, Eq)

newtype DisplayName = DisplayName { getDisplayName :: T.Text } deriving (Show, Eq)

newtype Uuid = Uuid { getUuid :: T.Text } deriving (Show, Eq)

data Href = Href {
    hrefName :: !(Maybe T.Text)
  , hrefUrl  :: !Uri
  } deriving (Show, Eq)

data UserType =
    TeamUserType -- ^ "team"
  | UserUserType -- ^ "user"
    deriving (Show, Eq)

data User = User {
    userName        :: !Username
  , userDisplayName :: !DisplayName
  , userType        :: !UserType
  , userUuid        :: !Uuid
  , userAvatar      :: !Href
  , userHtml        :: !Href
  , userSelf        :: !Href
  } deriving (Show, Eq)

newtype ProjectName = ProjectName {
    getProjectName :: T.Text
  } deriving (Show, Eq)

newtype ProjectKey = ProjectKey {
    getProjectKey :: T.Text
  } deriving (Show, Eq)

-- |
-- @"type": "project"@
--
data Project = Project {
    projectName   :: !ProjectName
  , projectUuid   :: !Uuid
  , projectKey    :: !ProjectKey
  , projectAvatar :: !Href
  , projectHtml   :: !Href
  , projectSelf   :: !Href
  } deriving (Show, Eq)

newtype Language = Language { getLanguage :: T.Text } deriving (Show, Eq)

newtype Uri = Uri { getUri :: T.Text } deriving (Show, Eq)

newtype EmailMailingList = EmailMailingList {
    getEmailMailingList :: T.Text
  } deriving (Show, Eq)

newtype RepoName = RepoName { getRepoName :: T.Text } deriving (Show, Eq)

data Privacy =
    Private
  | Public
    deriving (Show, Eq)

newtype RepoSlug = RepoSlug { getSlug :: T.Text } deriving (Show, Eq)

data HasIssues =
    HasIssues
  | NoIssues
    deriving (Show, Eq)

newtype RepoDescription = RepoDescription { getDescription :: T.Text } deriving (Show, Eq)

newtype Website = Website { getWebsite :: T.Text } deriving (Show, Eq)

-- |
-- this apparently isnt used,
-- but its a text field, and its set in the values i am getting
-- back from the API, so we need to be able to keep its value...
--
newtype RepoState = RepoState { getRepoState :: T.Text } deriving (Show, Eq)

-- |
-- This is for mercurial repos only, and its supposed
-- to represent some sort of patch queue structure if
-- this repo is a patch queue,
-- It will be null for all git repos.
--
data MqOf =
  SomePatchStructure -- ^ Not Supported
  deriving (Show, Eq)

data ReadWriteMode =
   ReadWrite
 | ReadOnly
   deriving (Show, Eq)

-- |
-- Indicates whether the repo is a patch queue or not,
-- only applies to Mercurial repos,
-- is `NotPatchQueue` for all git repos
--
data MqType =
    IsPatchQueue
  | NotPatchQueue
    deriving (Show, Eq)

-- |
-- `ForkPermissions` and `PublicForkPermissions` are v1
-- in v2 they are combined into one type.
data ForkPolicy =
   NoForksPolicy
 | NoPublicForksPolicy
 | ForkAwayPolicy
   deriving (Show, Eq)

data HasWiki =
    HasWiki
  | NoWiki
    deriving (Show, Eq)

data Scm =
    Git
  | Mercurial
    deriving (Show, Eq)
