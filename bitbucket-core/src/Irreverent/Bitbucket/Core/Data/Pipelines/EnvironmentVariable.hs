{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitucket.Core.Data.Pipelines.EnvironmentVariable
-- Copyright    : (C) 2017 - 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Core.Data.Pipelines.EnvironmentVariable (
  -- * Types
    PipelinesEnvironmentVariable(..)
  , PipelinesEnvironmentVariableValue(..)
  ) where

import Irreverent.Bitbucket.Core.Data.Common (
    Uuid(..)
  )

import qualified Ultra.Data.Text as T

import Preamble

data PipelinesEnvironmentVariableValue =
    SecuredPipelinesVariableValue
  | UnsecuredPipelinesVariableValue !T.Text
    deriving (Show, Eq)

data PipelinesEnvironmentVariable = PipelinesEnvironmentVariable {
    pevValue    :: !PipelinesEnvironmentVariableValue
  , pevKey      :: !T.Text
  , pevUuid     :: !Uuid
  } deriving (Show, Eq)
