{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Json.Pipelines.Config
-- Copyright    : (C) 2017 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Json.Pipelines.Config (
  -- * Types
    PipelinesConfigJsonV2(..)
  -- * Functions
  , pipelineConfigFromJson
  ) where

import Irreverent.Bitbucket.Json.RepositorySummary

import Irreverent.Bitbucket.Core.Data.Pipelines.Config (PipelinesConfig(..))

import Ultra.Data.Aeson (FromJSON(..), ToJSON(..), (.=), (.:), object)

import Preamble

newtype PipelinesConfigJsonV2 = PipelinesConfigJsonV2 {
    fromPipelinesConfigJsonV2 :: PipelinesConfig
  } deriving (Show, Eq)

pipelineConfigFromJson :: PipelinesConfigJsonV2 -> PipelinesConfig
pipelineConfigFromJson = fromPipelinesConfigJsonV2

instance ToJSON PipelinesConfigJsonV2 where
--toJSON :: a -> Value
  toJSON (PipelinesConfigJsonV2 cfg) =
    object [
        "enabled" .= pcEnabled cfg
      , "repository" .= RepositorySummaryJsonV2 (pcRepo cfg)
      ]

instance FromJSON PipelinesConfigJsonV2 where
--parseJSON :: Value -> Parser a
  parseJSON v = PipelinesConfigJsonV2 <$> do
    o <- parseJSON v
    PipelinesConfig
      <$> (o .: "enabled")
      <*> (repoSummaryFromJson <$> (o .: "repository"))






