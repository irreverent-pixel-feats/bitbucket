{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Json.Pipelines.SSHKeyPair
-- Copyright    : (C) 2017 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Json.Pipelines.SSHKeyPair (
  -- * Types
    PipelinesSSHKeyPairJsonV2(..)
  -- * Functions
  , pipelineSSHKeyPairFromJson
  ) where

import Irreverent.Bitbucket.Json.Common

import Irreverent.Bitbucket.Core.Data.Pipelines.SSHKeyPair (
    PipelinesSSHKeyPair(..)
  )

import Ultra.Data.Aeson (FromJSON(..), ToJSON(..), (.=), (.:), object)

import Preamble

newtype PipelinesSSHKeyPairJsonV2 = PipelinesSSHKeyPairJsonV2 {
    fromPipelinesSSHKeyPairJsonV2 :: PipelinesSSHKeyPair
  } deriving (Show, Eq)

pipelineSSHKeyPairFromJson :: PipelinesSSHKeyPairJsonV2 -> PipelinesSSHKeyPair
pipelineSSHKeyPairFromJson = fromPipelinesSSHKeyPairJsonV2

instance ToJSON PipelinesSSHKeyPairJsonV2 where
--toJSON :: a -> Value
  toJSON (PipelinesSSHKeyPairJsonV2 pair) = object [
      "public" .= jsonPublicSSHKey (pskpPublicKey pair)
    ]

instance FromJSON PipelinesSSHKeyPairJsonV2 where
--parseJSON :: Value -> Parser a
  parseJSON v = PipelinesSSHKeyPairJsonV2 <$> do
    o <- parseJSON v
    PipelinesSSHKeyPair
      <$> (o .: "public" >>= parsePublicSSHKey)




