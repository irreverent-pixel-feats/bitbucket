{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Json.Group
-- Copyright    : (C) 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Json.Group (
  -- * Functions
    jsonGroupV1
  , parseGroupV1
  ) where

import Irreverent.Bitbucket.Json.Common

import Irreverent.Bitbucket.Core.Data.Group (GroupV1(..))

import Ultra.Data.Aeson (
    Parser
  , Object
  , Value(..)
  , (.=)
  , (.:)
  , object
  )

import Preamble

jsonGroupV1 :: GroupV1 -> Value
jsonGroupV1 (GroupV1 owner nm members slug) = object [
    "owner" .= jsonUserV1 owner
  , "name" .= jsonGroupName nm
  , "members" .= (jsonUserV1 <$> members)
  , "slug" .= jsonGroupSlug slug
  ]

parseGroupV1 :: Object -> Parser GroupV1
parseGroupV1 o = GroupV1
  <$> (o .: "owner" >>= parseUserV1)
  <*> (o .: "name" >>= parseGroupName)
  <*> (o .: "members" >>= traverse parseUserV1)
  <*> (o .: "slug" >>= parseGroupSlug)
