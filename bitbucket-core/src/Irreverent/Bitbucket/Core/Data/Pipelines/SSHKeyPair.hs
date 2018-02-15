{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitucket.Core.Data.Pipelines.SSHKeyPair
-- Copyright    : (C) 2017 - 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Core.Data.Pipelines.SSHKeyPair (
  -- * Types
    PipelinesSSHKeyPair(..)
  ) where

import Irreverent.Bitbucket.Core.Data.Common (
    PublicSSHKey(..)
  )

import Preamble

data PipelinesSSHKeyPair = PipelinesSSHKeyPair {
    pskpPublicKey  :: !PublicSSHKey
  } deriving (Show, Eq)
