{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Json.RepositorySummary
-- Copyright    : (C) 2017 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Json.RepositorySummary (
  -- * Types
    RepositorySummaryJsonV2(..)
  -- * Functions
  , repoSummaryFromJson
  ) where

import Irreverent.Bitbucket.Json.Common

import Irreverent.Bitbucket.Core.Data.RepositorySummary (RepositorySummary(..))

import Ultra.Data.Aeson (FromJSON(..), ToJSON(..), (.=), (.:), object)

import Preamble

newtype RepositorySummaryJsonV2 = RepositorySummaryJsonV2 {
    fromRepositorySummaryJsonV2 :: RepositorySummary
  } deriving (Show, Eq)

repoSummaryFromJson :: RepositorySummaryJsonV2 -> RepositorySummary
repoSummaryFromJson = fromRepositorySummaryJsonV2

instance ToJSON RepositorySummaryJsonV2 where
--toJSON :: a -> Value
  toJSON (RepositorySummaryJsonV2 repo) =
    object [
        "full_name" .= jsonRepoName (rsFullName repo)
      , "name" .= jsonRepoName (rsName repo)
      , "uuid" .= jsonUuid (rsUuid repo)
      , "links" .= object [
          "avatar" .= jsonHref (rsAvatar repo)
        , "html" .= jsonHref (rsHtml repo)
        , "self" .= jsonHref (rsSelf repo)
        ]
      ]

instance FromJSON RepositorySummaryJsonV2 where
--parseJSON :: Value -> Parser a
  parseJSON v = RepositorySummaryJsonV2 <$> do
    o <- parseJSON v
    links <- o .: "links"
    RepositorySummary
      <$> (o .: "full_name" >>= parseRepoName)
      <*> (o .: "name" >>= parseRepoName)
      <*> (links .: "self" >>= parseHrefJson)
      <*> (links .: "html" >>= parseHrefJson)
      <*> (links .: "avatar" >>= parseHrefJson)
      <*> (o .: "uuid" >>= parseUuidJson)






