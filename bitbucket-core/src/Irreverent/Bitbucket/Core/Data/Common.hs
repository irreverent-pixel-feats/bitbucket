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
  , GitURLType(..)
  , DisplayName(..)
  , EmailMailingList(..)
  , Language(..)
  , GroupOwner(..)
  , GroupName(..)
  , GroupSlug(..)
  , HasIssues(..)
  , HasWiki(..)
  , Href(..)
  , Scm(..)
  , User(..)
  , Username(..)
  , UserType(..)
  , UserV1(..)
  , Uri(..)
  , ForkPolicy(..)
  , MqOf(..)
  , MqType(..)
  , PipelinesEnvironmentVariableSecurity(..)
  , Privacy(..)
  , PrivateSSHKey(..)
  , PrivilegeLevel(..)
  , Project(..)
  , ProjectKey(..)
  , ProjectName(..)
  , PublicSSHKey(..)
  , ReadWriteMode(..)
  , RepoDescription(..)
  , RepoName(..)
  , RepositoryV1(..)
  , RepoSlug(..)
  , RepoState(..)
  , Uuid(..)
  , Website(..)
  -- * Functions
  , privilegeLevelFromText
  , renderPrivilegeLevel
  ) where

import qualified Ultra.Data.Text as T

import Data.Time.Clock (UTCTime)

import Preamble

data GitURLType =
    HTTPSGitURLType
  | SSHGitURLType
    deriving (Show, Eq)

-- In v2 of the bitbucket API, this seems like a pretty compliant ISO8601 datetime
newtype BitbucketTime = BitbucketTime { bitbucketTime :: UTCTime } deriving (Show, Eq)

newtype Username = Username { getUsername :: T.Text } deriving (Show, Eq)

newtype GroupOwner = GroupOwner { groupOwner :: T.Text } deriving (Show, Eq)

newtype GroupName = GroupName { groupName :: T.Text } deriving (Show, Eq)

newtype GroupSlug = GroupSlug {
    groupSlug :: T.Text
  } deriving (Show, Eq)

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

-- | The User type for V1 of the Bitbucket REST API,
-- It contains less information.
data UserV1 = UserV1 {
    uV1Name       :: !Username
  , uV1FirstName  :: !DisplayName
  , uV1LastName   :: !DisplayName
  , uV1Avatar     :: !Uri
  , uv1Type       :: !UserType
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

data RepositoryV1 = RepositoryV1 {
    rV1Owner :: !UserV1
  , rV1Name :: !RepoName
  , rV1Slug :: !RepoSlug
  } deriving (Show, Eq)

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
-- Env vars can be secured, in which case the values
-- will never be exposed in the logs or the REST API.
-- They only get exposed to the pipeline builds.
data PipelinesEnvironmentVariableSecurity =
    SecuredVariable
  | UnsecuredVariable
    deriving (Show, Eq)

newtype PublicSSHKey = PublicSSHKey {
    publicKeyText :: T.Text
  } deriving (Show, Eq)

newtype PrivateSSHKey = PrivateSSHKey {
    privateKeyText :: T.Text
  } deriving (Show, Eq)

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

data PrivilegeLevel =
    ReadOnlyPrivilege
  | ReadWritePrivileges
  | AdminPrivileges
    deriving (Show, Eq)

renderPrivilegeLevel :: PrivilegeLevel -> T.Text
renderPrivilegeLevel ReadOnlyPrivilege = "read"
renderPrivilegeLevel ReadWritePrivileges = "write"
renderPrivilegeLevel AdminPrivileges = "admin"

privilegeLevelFromText :: T.Text -> Maybe PrivilegeLevel
privilegeLevelFromText "read"   = pure ReadOnlyPrivilege
privilegeLevelFromText "write"  = pure ReadWritePrivileges
privilegeLevelFromText "admin"  = pure AdminPrivileges
privilegeLevelFromText _        = Nothing
