{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Json.NewAccessKey
-- Copyright    : (C) 2017 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Json.NewAccessKey (
  -- * Types
    NewAccessKeyJsonV1(..)
  -- * Functions
  , newAccessKeyFromJson
  ) where

import Irreverent.Bitbucket.Json.Common

import Irreverent.Bitbucket.Core.Data.NewAccessKey

import Ultra.Data.Aeson (
    FromJSON(..)
  , ToJSON(..)
  , (.=)
  , (.:)
  , (.:?)
  , object
  )

import Preamble

newtype NewAccessKeyJsonV1 =
  NewAccessKeyJsonV1 NewAccessKey
  deriving (Show, Eq)

newAccessKeyFromJson :: NewAccessKeyJsonV1 -> NewAccessKey
newAccessKeyFromJson (NewAccessKeyJsonV1 x) = x

instance ToJSON NewAccessKeyJsonV1 where
--toJSON :: a -> Value
  toJSON (NewAccessKeyJsonV1 (NewAccessKey label key)) =
    object [
      "label" .= label
    , "key"   .= jsonPublicSSHKey key
    ]

instance FromJSON NewAccessKeyJsonV1 where
--parseJSON :: Value -> Parser a
  parseJSON v = NewAccessKeyJsonV1 <$> do
    o <- parseJSON v
    NewAccessKey
      <$> o .:? "label"
      <*> (o .: "key" >>= parsePublicSSHKey)

