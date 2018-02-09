{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Core.Data.Pipelines.UpdateConfig
-- Copyright    : (C) 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Core.Data.Pipelines.UpdateConfig (
  -- * Types
    UpdatePipelinesConfig(..)
  ) where

import Preamble

-- |
-- There might be more but the doco is a little hard to make sense of.
-- <https://developer.atlassian.com/bitbucket/api/2/reference/resource/repositories/%7Busername%7D/%7Brepo_slug%7D/pipelines_config#put>
-- Didn't seem to be much sense in sending all that repository info, and seems to work without it
-- So I will omit it.
-- Not sure if there's some needle in that haystack I have missed.
--
data UpdatePipelinesConfig = UpdatePipelinesConfig {
    upcEnabled :: !Bool
  } deriving (Show, Eq)
