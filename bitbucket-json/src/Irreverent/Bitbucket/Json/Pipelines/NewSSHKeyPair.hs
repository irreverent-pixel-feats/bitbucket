{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Json.Pipelines.NewSSHKeyPair
-- Copyright    : (C) 2017 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Json.Pipelines.NewSSHKeyPair (
  -- * Types
    PipelinesNewSSHKeyPairJsonV2(..)
  -- * Functions
  , pipelineNewSSHKeyPairFromJson
  ) where

import Irreverent.Bitbucket.Json.Common

import Irreverent.Bitbucket.Core.Data.Pipelines.NewSSHKeyPair (
    NewPipelinesSSHKeyPair(..)
  )

import Ultra.Data.Aeson (FromJSON(..), ToJSON(..), (.=), (.:), object)

import Preamble

newtype PipelinesNewSSHKeyPairJsonV2 = PipelinesNewSSHKeyPairJsonV2 {
    fromPipelinesNewSSHKeyPairJsonV2 :: NewPipelinesSSHKeyPair
  } deriving (Show, Eq)

pipelineNewSSHKeyPairFromJson :: PipelinesNewSSHKeyPairJsonV2 -> NewPipelinesSSHKeyPair
pipelineNewSSHKeyPairFromJson = fromPipelinesNewSSHKeyPairJsonV2

instance ToJSON PipelinesNewSSHKeyPairJsonV2 where
--toJSON :: a -> Value
  toJSON (PipelinesNewSSHKeyPairJsonV2 pair) = object [
      "private" .= jsonPrivateSSHKey (npskpPrivateKey pair)
    , "public" .= jsonPublicSSHKey (npskpPublicKey pair)
    ]

instance FromJSON PipelinesNewSSHKeyPairJsonV2 where
--parseJSON :: Value -> Parser a
  parseJSON v = PipelinesNewSSHKeyPairJsonV2 <$> do
    o <- parseJSON v
    NewPipelinesSSHKeyPair
      <$> (o .: "private" >>= parsePrivateSSHKey)
      <*> (o .: "public" >>= parsePublicSSHKey)




