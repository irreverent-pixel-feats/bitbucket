{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitucket.Core.Data.Pipelines.NewEnvironmentVariable
-- Copyright    : (C) 2017 - 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Core.Data.Pipelines.NewEnvironmentVariable (
  -- * Types
    NewPipelinesEnvironmentVariable(..)
  ) where

import Irreverent.Bitbucket.Core.Data.Common (
    PipelinesEnvironmentVariableSecurity(..)
  )

import qualified Ultra.Data.Text as T

import Preamble

data NewPipelinesEnvironmentVariable = NewPipelinesEnvironmentVariable {
    npevKey       :: !T.Text
  , npevValue     :: !T.Text
  , npevSecurity  :: !PipelinesEnvironmentVariableSecurity
  } deriving (Show, Eq)
