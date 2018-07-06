{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Json.Groups.Privileges
-- Copyright    : (C) 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Json.Groups.Privileges (
  -- * Types
    GroupPrivilegeV1Json(..)
  -- * Functions
  , fromGroupPrivilegeV1Json
  ) where

import Irreverent.Bitbucket.Json.Common
import Irreverent.Bitbucket.Json.Group

import Irreverent.Bitbucket.Core

import Ultra.Data.Aeson (
    FromJSON(..)
  , ToJSON(..)
  , Object
  , Parser
  , Value(..)
  , (.=)
  , (.:)
  , jsonTextEnum
  , object
  )

import Preamble

newtype GroupPrivilegeV1Json =
  GroupPrivilegeV1Json GroupPrivilegeV1
  deriving (Show, Eq)

fromGroupPrivilegeV1Json :: GroupPrivilegeV1Json -> GroupPrivilegeV1
fromGroupPrivilegeV1Json (GroupPrivilegeV1Json x) = x

jsonGroupPrivilegeV1 :: GroupPrivilegeV1 -> Value
jsonGroupPrivilegeV1 (GroupPrivilegeV1 repoName' priv grp repo) =
  object [
      "repo" .= jsonRepoName repoName'
    , "privilege" .= renderPrivilegeLevel priv
    , "group" .= jsonGroupV1 grp
    , "repository" .= jsonRepositoryV1 repo
    ]

parsePrivilegeLevel :: Value -> Parser PrivilegeLevel
parsePrivilegeLevel = jsonTextEnum [
    ("read", ReadOnlyPrivilege)
  , ("write", ReadWritePrivileges)
  , ("admin", AdminPrivileges)
  ]

parseGroupPrivilegeV1 :: Object -> Parser GroupPrivilegeV1
parseGroupPrivilegeV1 o = GroupPrivilegeV1
  <$> (o .: "repo" >>= parseRepoName)
  <*> (o .: "privilege" >>= parsePrivilegeLevel)
  <*> (o .: "group" >>= parseGroupV1)
  <*> (o .: "repository" >>= parseRepositoryV1)

instance ToJSON GroupPrivilegeV1Json where
--toJSON :: a -> Value
  toJSON (GroupPrivilegeV1Json priv) =
    jsonGroupPrivilegeV1 priv

instance FromJSON GroupPrivilegeV1Json where
--parseJSON :: Value -> Parser a
  parseJSON v = do
    o <- parseJSON v
    GroupPrivilegeV1Json <$> parseGroupPrivilegeV1 o
