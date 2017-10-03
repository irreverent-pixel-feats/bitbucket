{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Json.NewRepository
-- Copyright    : (C) 2017 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Json.NewRepository (
  -- * Types
    NewRepositoryJsonV2(..)
  -- * Functions
  , newRepoFromJson
  ) where

import Irreverent.Bitbucket.Json.Common

import Irreverent.Bitbucket.Core.Data.NewRepository

import Ultra.Data.Aeson (FromJSON(..), ToJSON(..), (.=), (.:), object)

import Preamble

newtype NewRepositoryJsonV2 =
  NewRepositoryJsonV2 NewRepository
  deriving (Show, Eq)

newRepoFromJson :: NewRepositoryJsonV2 -> NewRepository
newRepoFromJson (NewRepositoryJsonV2 x) = x

instance ToJSON NewRepositoryJsonV2 where
--toJSON :: a -> Value
  toJSON (NewRepositoryJsonV2 (NewRepository desc scm' forkPolicy privacy language hasWiki hasIssues)) =
    object [
      "scm"         .= jsonScm scm'
    , "has_wiki"    .= jsonHasWiki hasWiki
    , "has_issues"  .= jsonHasIssues hasIssues
    , "is_private"  .= jsonPrivacy privacy
    , "fork_policy" .= jsonForkPolicy forkPolicy
    , "description" .= jsonRepoDescription desc
    , "language"    .= jsonLanguage language
    ]

instance FromJSON NewRepositoryJsonV2 where
--parseJSON :: Value -> Parser a
  parseJSON v = NewRepositoryJsonV2 <$> do
    o <- parseJSON v
    NewRepository
      <$> (o .: "description" >>= parseRepoDescriptionJson)
      <*> (o .: "scm" >>= parseScmJson)
      <*> (o .: "fork_policy" >>= parseForkPolicyJson)
      <*> (o .: "is_private" >>= parsePrivacyJson)
      <*> (o .: "language" >>= parseLanguageJson)
      <*> (o .: "has_wiki" >>= parseHasWikiJson)
      <*> (o .: "has_issues" >>= parseHasIssuesJson)

