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
  , environmentVariables
  , environmentVariableValues
  , forkPolicies
  , hasIssues
  , hasWikis
  , hrefs
  , languages
  , accessKeys
  , newAccessKeys
  , newRepositories
  , newEnvironmentVariables
  , newSSHKeyPairs
  , privacies
  , privateSSHKeys
  , publicSSHKeys
  , pipelineEnvVarSecurity
  , repoDescriptions
  , repoNames
  , repositories
  , repositorySummaries
  , pipelineConfigs
  , updatePipelineConfigs
  , scms
  , sshKeyPairs
  , uuids
  , uris
  , websites
  ) where

import Irreverent.Bitbucket.Core

import Lab.Core.Gen (maybeOf, textOf, alphaNumChars)
import Lab.Core.QuickCheck (
    Arbitrary(..)
  , Gen
  , NonNegative(..)
  , choose
  , elements
  , frequency
  )

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
  <*> maybeOf bitbucketProjectKeys
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

repositorySummaries :: Gen RepositorySummary
repositorySummaries = RepositorySummary
  <$> repoNames
  <*> repoNames
  <*> hrefs
  <*> hrefs
  <*> hrefs
  <*> uuids

pipelineConfigs :: Gen PipelinesConfig
pipelineConfigs = PipelinesConfig
  <$> arbitrary
  <*> repositorySummaries

updatePipelineConfigs :: Gen UpdatePipelinesConfig
updatePipelineConfigs = UpdatePipelinesConfig
  <$> arbitrary

publicSSHKeys :: Gen PublicSSHKey
publicSSHKeys = PublicSSHKey <$> textOf alphaNumChars

privateSSHKeys :: Gen PrivateSSHKey
privateSSHKeys = PrivateSSHKey <$> textOf alphaNumChars

pipelineEnvVarSecurity :: Gen PipelinesEnvironmentVariableSecurity
pipelineEnvVarSecurity = elements [
    SecuredVariable
  , UnsecuredVariable
  ]

environmentVariableValues :: Gen PipelinesEnvironmentVariableValue
environmentVariableValues = frequency [
    (1, pure SecuredPipelinesVariableValue)
  , (4, UnsecuredPipelinesVariableValue <$> textOf alphaNumChars)
  ]

newEnvironmentVariables :: Gen NewPipelinesEnvironmentVariable
newEnvironmentVariables = NewPipelinesEnvironmentVariable
  <$> textOf alphaNumChars
  <*> textOf alphaNumChars
  <*> pipelineEnvVarSecurity

environmentVariables :: Gen PipelinesEnvironmentVariable
environmentVariables = PipelinesEnvironmentVariable
  <$> environmentVariableValues
  <*> textOf alphaNumChars
  <*> uuids

newSSHKeyPairs :: Gen NewPipelinesSSHKeyPair
newSSHKeyPairs = NewPipelinesSSHKeyPair
  <$> privateSSHKeys
  <*> publicSSHKeys

sshKeyPairs :: Gen PipelinesSSHKeyPair
sshKeyPairs = PipelinesSSHKeyPair
  <$> publicSSHKeys

newAccessKeys :: Gen NewAccessKey
newAccessKeys = NewAccessKey
  <$> maybeOf (textOf alphaNumChars)
  <*> publicSSHKeys

accessKeys :: Gen AccessKey
accessKeys = AccessKey
  <$> maybeOf (textOf alphaNumChars)
  <*> publicSSHKeys
  <*> choose (100000, 999999)
