{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Core.Data.Group
-- Copyright    : (C) 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Core.Data.Group (
  -- * Types
    GroupV1(..)
  ) where

import Irreverent.Bitbucket.Core.Data.Common

import Preamble

-- | The Group information
-- that is provided/required by V1 of the bitbucket cloud rest api
-- Note that V1 of the REST API is deprecated and will be gone by Dec 2018
--
data GroupV1 = GroupV1 {
    gV1Owner    :: !UserV1
  , gV1Name     :: !GroupName
  , gV1Members  :: ![UserV1]
  , gV1Slug     :: !GroupSlug
  } deriving (Show, Eq)
