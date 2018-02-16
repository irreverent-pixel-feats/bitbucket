{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Json.Pipelines.EnvironmentVariable
-- Copyright    : (C) 2017 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Json.Pipelines.EnvironmentVariable (
  -- * Types
    PipelinesEnvironmentVariableJsonV2(..)
  -- * Functions
  , pipelineEnvironmentVariableFromJson
  ) where

import Irreverent.Bitbucket.Json.Common

import Irreverent.Bitbucket.Core.Data.Pipelines.EnvironmentVariable (
    PipelinesEnvironmentVariable(..)
  , PipelinesEnvironmentVariableValue(..)
  )

import Ultra.Data.Aeson (FromJSON(..), ToJSON(..), (.=), (.:), object)
import qualified Ultra.Data.Text as T

import Preamble

newtype PipelinesEnvironmentVariableJsonV2 = PipelinesEnvironmentVariableJsonV2 {
    fromPipelinesEnvironmentVariableJsonV2 :: PipelinesEnvironmentVariable
  } deriving (Show, Eq)

pipelineEnvironmentVariableFromJson :: PipelinesEnvironmentVariableJsonV2 -> PipelinesEnvironmentVariable
pipelineEnvironmentVariableFromJson = fromPipelinesEnvironmentVariableJsonV2

instance ToJSON PipelinesEnvironmentVariableJsonV2 where
--toJSON :: a -> Value
  toJSON (PipelinesEnvironmentVariableJsonV2 var) =
    case pevValue var of
      SecuredPipelinesVariableValue -> object [
          "value" .= (mempty :: T.Text)
        , "key" .= pevKey var
        , "uuid" .= (jsonUuid . pevUuid $ var)
        , "secured" .= True
        ]
      UnsecuredPipelinesVariableValue val -> object [
          "value" .= val
        , "key" .= pevKey var
        , "uuid" .= (jsonUuid . pevUuid $ var)
        , "secured" .= False
        ]

instance FromJSON PipelinesEnvironmentVariableJsonV2 where
--parseJSON :: Value -> Parser a
  parseJSON v = PipelinesEnvironmentVariableJsonV2 <$> do
    o <- parseJSON v
    secured <- o .: "secured"
    value <- case secured of
      True   -> pure SecuredPipelinesVariableValue
      False  -> UnsecuredPipelinesVariableValue <$> o .: "value"
    PipelinesEnvironmentVariable
      <$> pure value
      <*> o .: "key"
      <*> (o .: "uuid" >>= parseUuidJson)




