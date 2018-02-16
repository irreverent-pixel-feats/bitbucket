{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Json.AccessKey
-- Copyright    : (C) 2017 - 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Json.AccessKey (
  -- * Types
    AccessKeyJsonV1(..)
  -- * Functions
  , accessKeyFromJson
  ) where

import Irreverent.Bitbucket.Json.Common

import Irreverent.Bitbucket.Core.Data.AccessKey

import Ultra.Data.Aeson (
    FromJSON(..)
  , ToJSON(..)
  , (.=)
  , (.:)
  , (.:?)
  , object
  )

import Preamble

newtype AccessKeyJsonV1 =
  AccessKeyJsonV1 AccessKey
  deriving (Show, Eq)

accessKeyFromJson :: AccessKeyJsonV1 -> AccessKey
accessKeyFromJson (AccessKeyJsonV1 x) = x

instance ToJSON AccessKeyJsonV1 where
--toJSON :: a -> Value
  toJSON (AccessKeyJsonV1 (AccessKey label key pk)) =
    object [
      "label" .= label
    , "key"   .= jsonPublicSSHKey key
    , "pk"    .= pk
    ]

instance FromJSON AccessKeyJsonV1 where
--parseJSON :: Value -> Parser a
  parseJSON v = AccessKeyJsonV1 <$> do
    o <- parseJSON v
    AccessKey
      <$> o .:? "label"
      <*> (o .: "key" >>= parsePublicSSHKey)
      <*> o .: "pk"

