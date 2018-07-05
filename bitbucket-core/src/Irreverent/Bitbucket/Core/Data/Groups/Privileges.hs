{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Core.Data.Groups.Privileges
-- Copyright    : (C) 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Core.Data.Groups.Privileges (
  -- * Types
    GroupPrivilegeV1(..)
  ) where

import Irreverent.Bitbucket.Core.Data.Common
import Irreverent.Bitbucket.Core.Data.Group

import Preamble

data GroupPrivilegeV1 = GroupPrivilegeV1 {
    gpV1RepoName  :: !RepoName
  , gpV1Privilege :: !PrivilegeLevel
  , gpV1Group     :: !GroupV1
  , gpV1Repo      :: !RepositoryV1
  } deriving (Show, Eq)

