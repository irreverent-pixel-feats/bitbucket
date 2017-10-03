{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Json.Repository
-- Copyright    : (C) 2017 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Json.Repository (
  -- * Types
    RepositoryJsonV2(..)
  -- * Functions
  , repoFromJson
  ) where

import Irreverent.Bitbucket.Json.Common

import Irreverent.Bitbucket.Core.Data.Common (Href(..))
import Irreverent.Bitbucket.Core.Data.Repository (Repository(..))

import Ultra.Data.Aeson (FromJSON(..), ToJSON(..), (.=), (.:), (.:?),  object)

import qualified Data.List as L

import Preamble

newtype RepositoryJsonV2 = RepositoryJsonV2 {
    fromRepositoryJsonV2 :: Repository
  } deriving (Show, Eq)

repoFromJson :: RepositoryJsonV2 -> Repository
repoFromJson = fromRepositoryJsonV2

instance ToJSON RepositoryJsonV2 where
--toJSON :: a -> Value
  toJSON (RepositoryJsonV2 repo) =
    object [
        "owner" .= fmap jsonUser (repoCreator repo)
      , "language" .= jsonLanguage (repoLanguage repo)
      , "project" .= (jsonProject <$> repoProject repo)
      , "full_name" .= jsonRepoName (repoFullName repo)
      , "name" .= jsonRepoName (repoName repo)
      , "is_private" .= jsonPrivacy (repoPrivacy repo)
      , "uuid" .= jsonUuid (repoUuid repo)
      , "fork_policy" .= jsonForkPolicy (repoForkPolicy repo)
      , "has_issues" .= jsonHasIssues (repoHasIssues repo)
      , "has_wiki" .= jsonHasWiki (repoHasWiki repo)
      , "description" .= jsonRepoDescription (repoDescription repo)
      , "website" .= fmap jsonWebsite (repoWebsite repo)
      , "created_on" .= jsonBitbucketTime (repoCreatedOn repo)
      , "size" .= (repoSize repo)
      , "last_updated" .= (jsonBitbucketTime <$> repoLastUpdated repo)
      , "scm" .= jsonScm (repoScm repo)
      , "links" .= object [
          "pullrequests" .= jsonHref (repoPRs repo)
        , "downloads" .= jsonHref (repoDownloads repo)
        , "forks" .= jsonHref (repoForks repo)
        , "hooks" .= jsonHref (repoHooks repo)
        , "avatar" .= jsonHref (repoAvatar repo)
        , "html" .= jsonHref (repoHtml repo)
        , "self" .= jsonHref (repoSelf repo)
        , "clone" .= fmap jsonHref [repoCloneHttps repo, repoCloneSsh repo]
        , "commits" .= jsonHref (repoCommits repo)
        , "tags" .= jsonHref (repoTags repo)
        , "branches" .= jsonHref (repoBranches repo)
        , "watchers" .= jsonHref (repoWatchers repo)
        ]
      ]

instance FromJSON RepositoryJsonV2 where
--parseJSON :: Value -> Parser a
  parseJSON v = RepositoryJsonV2 <$> do
    o <- parseJSON v
    links <- o .: "links"
    clones <- links .: "clone" >>= traverse parseHrefJson
    let cloneMap = fmap ((,) <$> hrefName <*> id) clones
    httpsClone <- maybe (fail "Could not find https clone href") pure $ L.lookup (pure "https") cloneMap
    sshClone <- maybe (fail "Could not find ssh clone href") pure $ L.lookup (pure "ssh") cloneMap
    Repository
      <$> (o .:? "owner" >>= traverse parseUserJson)
      <*> (o .: "language" >>= parseLanguageJson)
      <*> (o .:? "project" >>= traverse parseProjectJson)
      <*> (o .: "full_name" >>= parseRepoName)
      <*> (o .: "is_private" >>= parsePrivacyJson)
      <*> (o .: "uuid" >>= parseUuidJson)
      <*> (o .: "fork_policy" >>= parseForkPolicyJson)
      <*> (o .: "has_issues" >>= parseHasIssuesJson)
      <*> (o .: "description" >>= parseRepoDescriptionJson)
      <*> (o .:? "website" >>= traverse parseWebSiteJson)
      <*> (o .: "created_on" >>= parseBitbucketTimeJson)
      <*> o .: "size"
      <*> (o .:? "last_updated" >>= traverse parseBitbucketTimeJson)
      <*> (o .: "has_wiki" >>= parseHasWikiJson)
      <*> (o .: "scm" >>= parseScmJson)
      <*> (links .: "pullrequests" >>= parseHrefJson)
      <*> (links .: "downloads" >>= parseHrefJson)
      <*> (links .: "forks" >>= parseHrefJson)
      <*> (links .: "hooks" >>= parseHrefJson)
      <*> (links .: "avatar" >>= parseHrefJson)
      <*> (links .: "html" >>= parseHrefJson)
      <*> (links .: "self" >>= parseHrefJson)
      <*> pure httpsClone
      <*> pure sshClone
      <*> (links .: "commits" >>= parseHrefJson)
      <*> (links .: "tags" >>= parseHrefJson)
      <*> (links .: "branches" >>= parseHrefJson)
      <*> (links .: "watchers" >>= parseHrefJson)
      <*> (o .: "name" >>= parseRepoName)






