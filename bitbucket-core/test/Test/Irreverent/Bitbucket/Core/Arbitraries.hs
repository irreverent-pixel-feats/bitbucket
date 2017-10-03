{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Irreverent.Bitbucket.Core.Arbitraries (
  -- * Generators
    bitbucketDisplayNames
  , bitbucketProjects
  , bitbucketProjectKeys
  , bitbucketProjectNames
  , bitbucketTimes
  , bitbucketUsernames
  , bitbucketUsers
  , bitbucketUserTypes
  , forkPolicies
  , hasIssues
  , hasWikis
  , hrefs
  , languages
  , newRepositories
  , privacies
  , repoDescriptions
  , repoNames
  , repositories
  , scms
  , uuids
  , uris
  , websites
  ) where

import Irreverent.Bitbucket.Core.Data.Common
import Irreverent.Bitbucket.Core.Data.NewRepository
import Irreverent.Bitbucket.Core.Data.Repository

import Lab.Core.Gen (maybeOf, textOf, alphaNumChars)
import Lab.Core.QuickCheck (Arbitrary(..), Gen, NonNegative(..), elements)

import Test.QuickCheck.Instances ()

import Preamble

repoDescriptions :: Gen RepoDescription
repoDescriptions = RepoDescription <$> textOf alphaNumChars

scms :: Gen Scm
scms = elements [Git, Mercurial]

forkPolicies :: Gen ForkPolicy
forkPolicies = elements [NoForksPolicy, NoPublicForksPolicy, ForkAwayPolicy]

privacies :: Gen Privacy
privacies = elements [Public, Private]

languages :: Gen Language
languages = Language <$> elements ["", "haskell", "scala", "python", "lua"]

hasWikis :: Gen HasWiki
hasWikis = elements [HasWiki, NoWiki]

hasIssues :: Gen HasIssues
hasIssues = elements [HasIssues, NoIssues]

bitbucketUsernames :: Gen Username
bitbucketUsernames = Username <$> textOf alphaNumChars

bitbucketDisplayNames :: Gen DisplayName
bitbucketDisplayNames = DisplayName <$> textOf alphaNumChars

bitbucketUserTypes :: Gen UserType
bitbucketUserTypes = elements [TeamUserType, UserUserType]

uuids :: Gen Uuid
uuids = Uuid <$> textOf alphaNumChars

uris :: Gen Uri
uris = Uri <$> textOf alphaNumChars

hrefs :: Gen Href
hrefs = Href
  <$> maybeOf (textOf alphaNumChars)
  <*> uris

bitbucketUsers :: Gen User
bitbucketUsers = User
  <$> bitbucketUsernames
  <*> bitbucketDisplayNames
  <*> bitbucketUserTypes
  <*> uuids
  <*> hrefs
  <*> hrefs
  <*> hrefs

bitbucketProjectNames :: Gen ProjectName
bitbucketProjectNames = ProjectName <$> textOf alphaNumChars

bitbucketProjectKeys :: Gen ProjectKey
bitbucketProjectKeys = ProjectKey <$> textOf alphaNumChars

bitbucketProjects :: Gen Project
bitbucketProjects = Project
  <$> bitbucketProjectNames
  <*> uuids
  <*> bitbucketProjectKeys
  <*> hrefs
  <*> hrefs
  <*> hrefs

repoNames :: Gen RepoName
repoNames = RepoName <$> textOf alphaNumChars

websites :: Gen Website
websites = Website <$> textOf alphaNumChars

bitbucketTimes :: Gen BitbucketTime
bitbucketTimes = BitbucketTime <$> arbitrary

newRepositories :: Gen NewRepository
newRepositories = NewRepository
  <$> repoDescriptions
  <*> scms
  <*> forkPolicies
  <*> privacies
  <*> languages
  <*> hasWikis
  <*> hasIssues

repositories :: Gen Repository
repositories = Repository
  <$> maybeOf bitbucketUsers
  <*> languages
  <*> maybeOf bitbucketProjects
  <*> repoNames
  <*> privacies
  <*> uuids
  <*> forkPolicies
  <*> hasIssues
  <*> repoDescriptions
  <*> maybeOf websites
  <*> bitbucketTimes
  <*> (getNonNegative <$> arbitrary)
  <*> maybeOf bitbucketTimes
  <*> hasWikis
  <*> scms
  <*> hrefs
  <*> hrefs
  <*> hrefs
  <*> hrefs
  <*> hrefs
  <*> hrefs
  <*> hrefs
  <*> (Href <$> pure (pure "https") <*> uris)
  <*> (Href <$> pure (pure "ssh") <*> uris)
  <*> hrefs
  <*> hrefs
  <*> hrefs
  <*> hrefs
  <*> repoNames
