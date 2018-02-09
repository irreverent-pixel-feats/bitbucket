{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Json.Pipelines.UpdateConfig
-- Copyright    : (C) 2017 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Json.Pipelines.UpdateConfig (
  -- * Types
    UpdatePipelinesConfigJsonV2(..)
  -- * Functions
  , updatePipelineConfigFromJson
  ) where

import Irreverent.Bitbucket.Core.Data.Pipelines.UpdateConfig (UpdatePipelinesConfig(..))

import Ultra.Data.Aeson (FromJSON(..), ToJSON(..), (.=), (.:), object)

import Preamble

newtype UpdatePipelinesConfigJsonV2 = UpdatePipelinesConfigJsonV2 {
    fromUpdatePipelinesConfigJsonV2 :: UpdatePipelinesConfig
  } deriving (Show, Eq)

updatePipelineConfigFromJson :: UpdatePipelinesConfigJsonV2 -> UpdatePipelinesConfig
updatePipelineConfigFromJson = fromUpdatePipelinesConfigJsonV2

instance ToJSON UpdatePipelinesConfigJsonV2 where
--toJSON :: a -> Value
  toJSON (UpdatePipelinesConfigJsonV2 cfg) =
    object [
        "enabled" .= upcEnabled cfg
      ]

instance FromJSON UpdatePipelinesConfigJsonV2 where
--parseJSON :: Value -> Parser a
  parseJSON v = UpdatePipelinesConfigJsonV2 <$> do
    o <- parseJSON v
    UpdatePipelinesConfig
      <$> (o .: "enabled")






