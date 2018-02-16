{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Json.Pipelines.NewEnvironmentVariable
-- Copyright    : (C) 2017 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Json.Pipelines.NewEnvironmentVariable (
  -- * Types
    PipelinesNewEnvironmentVariableJsonV2(..)
  -- * Functions
  , pipelineNewEnvironmentVariableFromJson
  ) where

import Irreverent.Bitbucket.Json.Common

import Irreverent.Bitbucket.Core.Data.Pipelines.NewEnvironmentVariable (
    NewPipelinesEnvironmentVariable(..)
  )

import Ultra.Data.Aeson (FromJSON(..), ToJSON(..), (.=), (.:), object)

import Preamble

newtype PipelinesNewEnvironmentVariableJsonV2 = PipelinesNewEnvironmentVariableJsonV2 {
    fromPipelinesNewEnvironmentVariableJsonV2 :: NewPipelinesEnvironmentVariable
  } deriving (Show, Eq)

pipelineNewEnvironmentVariableFromJson :: PipelinesNewEnvironmentVariableJsonV2 -> NewPipelinesEnvironmentVariable
pipelineNewEnvironmentVariableFromJson = fromPipelinesNewEnvironmentVariableJsonV2

instance ToJSON PipelinesNewEnvironmentVariableJsonV2 where
--toJSON :: a -> Value
  toJSON (PipelinesNewEnvironmentVariableJsonV2 var) = object [
      "key" .= npevKey var
    , "value" .= npevValue var
    , "secured" .= jsonPipelineEnvVarSecurity (npevSecurity var)
    ]

instance FromJSON PipelinesNewEnvironmentVariableJsonV2 where
--parseJSON :: Value -> Parser a
  parseJSON v = PipelinesNewEnvironmentVariableJsonV2 <$> do
    o <- parseJSON v
    NewPipelinesEnvironmentVariable
      <$> o .: "key"
      <*> o .: "value"
      <*> (o .: "secured" >>= parsePipelineEnvSecurity)




