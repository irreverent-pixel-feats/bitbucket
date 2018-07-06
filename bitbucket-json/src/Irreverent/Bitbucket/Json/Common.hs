{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Json.Common
-- Copyright    : (C) 2017 Irreverent Pixel Feats
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Json.Common (
  -- * Serialise
    jsonBitbucketTime
  , jsonDisplayName
  , jsonEmailMailingList
  , jsonGroupName
  , jsonGroupOwner
  , jsonGroupSlug
  , jsonLanguage
  , jsonHasIssues
  , jsonHasWiki
  , jsonUri
  , jsonHref
  , jsonRepositoryV1
  , jsonScm
  , jsonUsername
  , jsonUuid
  , jsonUser
  , jsonUserV1
  , jsonUserType
  , jsonForkPolicy
  , jsonPipelineEnvVarSecurity
  , jsonPrivacy
  , jsonProjectKey
  , jsonSoloProjectKey
  , jsonProjectName
  , jsonProject
  , jsonPublicSSHKey
  , jsonPrivateSSHKey
  , jsonRepoDescription
  , jsonRepoName
  , jsonRepoSlug
  , jsonWebsite
  -- ** Deserialisers
  , parseBitbucketTimeJson
  , parseDisplayNameJson
  , parseEmailMailingListJson
  , parseGroupName
  , parseGroupOwner
  , parseGroupSlug
  , parseLanguageJson
  , parseHasIssuesJson
  , parseHasWikiJson
  , parseUriJson
  , parseHrefJson
  , parseScmJson
  , parseUsernameJson
  , parseUuidJson
  , parseUserTypeJson
  , parseUserJson
  , parseUserV1
  , parseForkPolicyJson
  , parsePrivacyJson
  , parseProjectKeyJson
  , parseSoloProjectKeyJson
  , parsePipelineEnvSecurity
  , parseProjectName
  , parseProjectJson
  , parsePublicSSHKey
  , parsePrivateSSHKey
  , parseRepoDescriptionJson
  , parseRepoName
  , parseRepoSlug
  , parseRepositoryV1
  , parseWebSiteJson
  ) where

import Irreverent.Bitbucket.Core.Data.Common (
    BitbucketTime(..)
  , DisplayName(..)
  , EmailMailingList(..)
  , ForkPolicy(..)
  , Language(..)
  , GroupName(..)
  , GroupOwner(..)
  , GroupSlug(..)
  , HasIssues(..)
  , HasWiki(..)
  , Href(..)
  , PipelinesEnvironmentVariableSecurity(..)
  , Privacy(..)
  , Project(..)
  , ProjectKey(..)
  , ProjectName(..)
  , PrivateSSHKey(..)
  , PublicSSHKey(..)
  , RepoDescription(..)
  , RepoName(..)
  , RepositoryV1(..)
  , RepoSlug(..)
  , Scm(..)
  , Uri(..)
  , User(..)
  , Username(..)
  , UserType(..)
  , UserV1(..)
  , Uuid(..)
  , Website(..)
  )

import Ultra.Data.Aeson (
    KeyValue
  , Parser
  , Object
  , Value(..)
  , (.=)
  , (.:)
  , (.:?)
  , object
  , parseJSON
  , toJSON
  , jsonTextEnum
  )
import qualified Ultra.Data.Text as T

import Data.Time.ISO8601 (formatISO8601, parseISO8601)

import Preamble

jsonBitbucketTime :: BitbucketTime -> Value
jsonBitbucketTime (BitbucketTime t) = toJSON . formatISO8601 $ t

parseBitbucketTimeJson :: Value -> Parser BitbucketTime
parseBitbucketTimeJson v = parseJSON v >>=
  maybe (fail "expected ISO8601 time") (pure . BitbucketTime) . parseISO8601 . T.unpack

jsonDisplayName :: DisplayName -> Value
jsonDisplayName (DisplayName n) = toJSON n

parseDisplayNameJson :: Value -> Parser DisplayName
parseDisplayNameJson v = DisplayName <$> parseJSON v

jsonEmailMailingList :: EmailMailingList -> Value
jsonEmailMailingList (EmailMailingList mailList) = toJSON mailList

parseEmailMailingListJson :: Value -> Parser EmailMailingList
parseEmailMailingListJson v = EmailMailingList <$> parseJSON v

jsonLanguage :: Language -> Value
jsonLanguage (Language l) = toJSON l

parseLanguageJson :: Value -> Parser Language
parseLanguageJson v = Language <$> parseJSON v

jsonHasIssues :: HasIssues -> Value
jsonHasIssues HasIssues = toJSON True
jsonHasIssues NoIssues = toJSON False

parseHasIssuesJson :: Value -> Parser HasIssues
parseHasIssuesJson v = flip fmap (parseJSON v) $ \case
  True  -> HasIssues
  False -> NoIssues

jsonHasWiki :: HasWiki -> Value
jsonHasWiki HasWiki = toJSON True
jsonHasWiki NoWiki  = toJSON False

parseHasWikiJson :: Value -> Parser HasWiki
parseHasWikiJson v = flip fmap (parseJSON v) $ \case
  True  -> HasWiki
  False -> NoWiki

jsonUri :: Uri -> Value
jsonUri (Uri uri) = toJSON uri

parseUriJson :: Value -> Parser Uri
parseUriJson v = Uri <$> parseJSON v

jsonHref :: Href -> Value
jsonHref (Href name url) =
  let
    base :: (KeyValue a) => [a]
    base = pure $ "href" .= jsonUri url
  in object . maybe id (\n -> (:) ("name" .= n)) name $ base

parseHrefJson :: Object -> Parser Href
parseHrefJson o = Href
  <$> o .:? "name"
  <*> (o .: "href" >>= parseUriJson)

jsonScm :: Scm -> Value
jsonScm scm = toJSON $ case scm of
  Git       -> "git" :: T.Text
  Mercurial -> "hg"

parseScmJson :: Value -> Parser Scm
parseScmJson = jsonTextEnum [("git", Git), ("hg", Mercurial)]

jsonUsername :: Username -> Value
jsonUsername (Username n) = toJSON n

parseUsernameJson :: Value -> Parser Username
parseUsernameJson v = Username <$> parseJSON v

jsonUuid :: Uuid -> Value
jsonUuid (Uuid u) = toJSON u

parseUuidJson :: Value -> Parser Uuid
parseUuidJson v = Uuid <$> parseJSON v

jsonUserType :: UserType -> Value
jsonUserType typ = toJSON $ case typ of
  TeamUserType -> "team" :: T.Text
  UserUserType -> "user"

parseUserTypeJson :: Value -> Parser UserType
parseUserTypeJson = jsonTextEnum [("team", TeamUserType), ("user", UserUserType)]

jsonUser :: User -> Value
jsonUser (User name dname typ uuid avatar html self) = object [
    "links" .= object [
      "avatar" .= jsonHref avatar
    , "html" .= jsonHref html
    , "self" .= jsonHref self
    ]
  , "uuid" .= jsonUuid uuid
  , "type" .= jsonUserType typ
  , "display_name" .= jsonDisplayName dname
  , "username" .= jsonUsername name
  ]

parseUserJson :: Object -> Parser User
parseUserJson o = do
  linksObject <- o .: "links"
  User
    <$> (o .: "username" >>= parseUsernameJson)
    <*> (o .: "display_name" >>= parseDisplayNameJson)
    <*> (o .: "type" >>= parseUserTypeJson)
    <*> (o .: "uuid" >>= parseUuidJson)
    <*> (linksObject .: "avatar" >>= parseHrefJson)
    <*> (linksObject .: "html" >>= parseHrefJson)
    <*> (linksObject .: "self" >>= parseHrefJson)

jsonForkPolicy :: ForkPolicy -> Value
jsonForkPolicy policy = toJSON $ case policy of
  NoForksPolicy       -> "no_forks" :: T.Text
  NoPublicForksPolicy -> "no_public_forks"
  ForkAwayPolicy      -> "allow_forks"

parseForkPolicyJson :: Value -> Parser ForkPolicy
parseForkPolicyJson = jsonTextEnum [
    ("no_forks", NoForksPolicy)
  , ("no_public_forks", NoPublicForksPolicy)
  , ("allow_forks", ForkAwayPolicy)
  ]

jsonPrivacy :: Privacy -> Value
jsonPrivacy p = toJSON $ case p of
  Private -> True
  Public  -> False

parsePrivacyJson :: Value -> Parser Privacy
parsePrivacyJson v = flip fmap (parseJSON v) $ \case
  True  -> Private
  False -> Public

-- |
-- In some messages, a project key appears in its own field,
-- but not as a string, but as a JSON object with a field "key"
jsonSoloProjectKey :: ProjectKey -> Value
jsonSoloProjectKey (ProjectKey key) = object [
    "key" .= key
  ]

parseSoloProjectKeyJson :: Object -> Parser ProjectKey
parseSoloProjectKeyJson o = ProjectKey <$> o .: "key"

jsonProjectKey :: ProjectKey -> Value
jsonProjectKey (ProjectKey key) = toJSON key

parseProjectKeyJson :: Value -> Parser ProjectKey
parseProjectKeyJson v = ProjectKey <$> parseJSON v

jsonPublicSSHKey :: PublicSSHKey -> Value
jsonPublicSSHKey (PublicSSHKey key) = toJSON key

parsePublicSSHKey :: Value -> Parser PublicSSHKey
parsePublicSSHKey v = PublicSSHKey <$> parseJSON v

jsonPrivateSSHKey :: PrivateSSHKey -> Value
jsonPrivateSSHKey (PrivateSSHKey key) = toJSON key

parsePrivateSSHKey :: Value -> Parser PrivateSSHKey
parsePrivateSSHKey v = PrivateSSHKey <$> parseJSON v

jsonPipelineEnvVarSecurity :: PipelinesEnvironmentVariableSecurity -> Value
jsonPipelineEnvVarSecurity SecuredVariable = toJSON True
jsonPipelineEnvVarSecurity UnsecuredVariable = toJSON False

parsePipelineEnvSecurity :: Value -> Parser PipelinesEnvironmentVariableSecurity
parsePipelineEnvSecurity v = flip fmap (parseJSON v) $ \case
  True  -> SecuredVariable
  False -> UnsecuredVariable

jsonProjectName :: ProjectName -> Value
jsonProjectName (ProjectName name) = toJSON name

parseProjectName :: Value -> Parser ProjectName
parseProjectName v = ProjectName <$> parseJSON v

jsonProject :: Project -> Value
jsonProject (Project name uuid key avatar html self) = object [
    "links" .= object [
        "avatar" .= jsonHref avatar
      , "html"   .= jsonHref html
      , "self"   .= jsonHref self
      ]
  , "name"  .= jsonProjectName name
  , "uuid"  .= jsonUuid uuid
  , "type"  .= ("project" :: T.Text)
  , "key"   .= jsonProjectKey key
  ]

parseProjectJson :: Object -> Parser Project
parseProjectJson o = do
  linksObject <- o .: "links"
  o .: "type" >>= jsonTextEnum [("project", ())]
  Project
    <$> (o .: "name" >>= parseProjectName)
    <*> (o .: "uuid" >>= parseUuidJson)
    <*> (o .: "key" >>= parseProjectKeyJson)
    <*> (linksObject .: "avatar" >>= parseHrefJson)
    <*> (linksObject .: "html" >>= parseHrefJson)
    <*> (linksObject .: "self" >>= parseHrefJson)

jsonRepoDescription :: RepoDescription -> Value
jsonRepoDescription (RepoDescription desc) = toJSON desc

parseRepoDescriptionJson :: Value -> Parser RepoDescription
parseRepoDescriptionJson v = RepoDescription <$> parseJSON v

jsonRepoName :: RepoName -> Value
jsonRepoName (RepoName repoName) = toJSON repoName

parseRepoName :: Value -> Parser RepoName
parseRepoName v = RepoName <$> parseJSON v

jsonGroupOwner :: GroupOwner -> Value
jsonGroupOwner (GroupOwner owner) = toJSON owner

parseGroupOwner :: Value -> Parser GroupOwner
parseGroupOwner = fmap GroupOwner . parseJSON

jsonGroupName :: GroupName -> Value
jsonGroupName (GroupName name) = toJSON name

parseGroupName :: Value -> Parser GroupName
parseGroupName = fmap GroupName . parseJSON

jsonGroupSlug :: GroupSlug -> Value
jsonGroupSlug (GroupSlug s) = toJSON s

parseGroupSlug :: Value -> Parser GroupSlug
parseGroupSlug = fmap GroupSlug . parseJSON

jsonUserV1 :: UserV1 -> Value
jsonUserV1 (UserV1 nm first' last' avatar typ) =
  let
    isTeam :: Bool
    isTeam = case typ of
      TeamUserType -> True
      UserUserType -> False
  in object [
      "username" .= jsonUsername nm
    , "first_name" .= jsonDisplayName first'
    , "last_name" .= jsonDisplayName last'
    , "avatar" .= jsonUri avatar
    , "is_team" .= isTeam
    ]

parseUserV1 :: Object -> Parser UserV1
parseUserV1 o =
  let
    userType' :: Bool -> UserType
    userType' True = TeamUserType
    userType' False = UserUserType

  in UserV1
    <$> (o .: "username" >>= parseUsernameJson) 
    <*> (o .: "first_name" >>= parseDisplayNameJson)
    <*> (o .: "last_name" >>= parseDisplayNameJson)
    <*> (o .: "avatar" >>= parseUriJson)
    <*> (userType' <$> o .: "is_team")

jsonRepositoryV1 :: RepositoryV1 -> Value
jsonRepositoryV1 (RepositoryV1 owner nm slug) = object [
    "owner" .= jsonUserV1 owner
  , "name" .= jsonRepoName nm
  , "slug" .= jsonRepoSlug slug
  ]

parseRepositoryV1 :: Object -> Parser RepositoryV1
parseRepositoryV1 o = RepositoryV1
  <$> (o .: "owner" >>= parseUserV1)
  <*> (o .: "name" >>= parseRepoName)
  <*> (o .: "slug" >>= parseRepoSlug)

jsonRepoSlug :: RepoSlug -> Value
jsonRepoSlug (RepoSlug slug) = toJSON slug

parseRepoSlug :: Value -> Parser RepoSlug
parseRepoSlug v = RepoSlug <$> parseJSON v

jsonWebsite :: Website -> Value
jsonWebsite (Website website) = toJSON website

parseWebSiteJson :: Value -> Parser Website
parseWebSiteJson v = Website <$> parseJSON v
