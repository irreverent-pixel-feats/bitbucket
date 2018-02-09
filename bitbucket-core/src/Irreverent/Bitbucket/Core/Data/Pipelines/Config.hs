{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Core.Data.Pipelines.Config
-- Copyright    : (C) 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Core.Data.Pipelines.Config (
    PipelinesConfig(..)
  ) where

import Irreverent.Bitbucket.Core.Data.RepositorySummary

import Preamble

data PipelinesConfig = PipelinesConfig {
    pcEnabled :: !Bool
  , pcRepo    :: !RepositorySummary
  } deriving (Show, Eq)
